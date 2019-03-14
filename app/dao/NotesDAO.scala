package dao

import java.io.File

import javax.inject.{Inject, Singleton}
import models._
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfig, HasDatabaseConfigProvider}
import util.MyPostgresDriver

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

trait NotesComponent { self: HasDatabaseConfig[MyPostgresDriver] =>
  import profile.api._

  class Notes(tag: Tag) extends Table[Note](tag, "NOTE") {

    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def text = column[String]("text")
    def color = column[String]("color")
    def fileName = column[Option[String]]("fileName")

    def * = (id, text, color, fileName) <> (Note.tupled, Note.unapply _)
  }
}

@Singleton
class NotesDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider, configuration: play.api.Configuration)(implicit ec: ExecutionContext)
  extends ColorsComponent with NotesComponent with TypesComponent with NoteTypeComponent with HasDatabaseConfigProvider[MyPostgresDriver] {
  import profile.api._

  private val notes = TableQuery[Notes]
  private val colors = TableQuery[Colors]
  private val types = TableQuery[Types]
  private val noteTypes = TableQuery[NoteTypes]

  def findById(id: Long): Future[Option[Note]] =
    db.run(notes.filter(_.id === id).result.headOption)

  def count(): Future[Int] =
    db.run(notes.length.result)

  def count(filterSeq: List[Long]): Future[Int] = {
    val query = notes
      .joinLeft(noteTypes)
      .on {
        case (n, nt) =>
          n.id === nt.noteId
      }
      .joinLeft(types)
      .on {
        case ((n, nt), t) =>
          nt.map(_.typeId) === t.id
      }
      .joinLeft(colors)
      .on {
        case (((n, nt), t), c) =>
          t.map(_.color) === c.id
      }
      .filter {
        case (((n, nt), t), c) =>
          Option(filterSeq.toSet)
            .filter(_.nonEmpty)
            .map(notEmptyFilter =>
              t.map(_.id.inSet(notEmptyFilter))
            )
            .getOrElse(Some(true): Rep[Option[Boolean]])
      }


    db.run(query.groupBy(x => x._1._1._1.id).map(c => c._1).subquery.length.result)
  }

  def getNotes(page: Int = 0, pageSize: Int = 10, orderBy: Int = 1): Future[Seq[Note]] = {
    val offset = pageSize * page
    db.run {
      notes.drop(offset).take(pageSize).result
    }
  }

  def findNotesWithTypes(
                          page: Int = 0,
                          pageSize: Int = 10,
                          sortField: SortableFields.Value,
                          sortOrder: SortOrder.Value,
                          filterSeq: List[Long],
                          textFilter: String = "%"
                        ): Future[Page[(Note, Seq[(String, String)])]] = {

    def sortingLogic(note: Notes) = {
      (sortField, sortOrder) match {
        case (SortableFields.text, SortOrder.asc) => note.text.toLowerCase.asc
        case (SortableFields.text, SortOrder.desc) => note.text.toLowerCase.desc
        case (SortableFields.file, SortOrder.asc) => note.fileName.toLowerCase.asc.nullsFirst
        case (SortableFields.file, SortOrder.desc) => note.fileName.toLowerCase.desc.nullsLast
      }
    }

    val offset = pageSize * page

    val mainQuery = notes
      .joinLeft(noteTypes)
      .on {
        case (n, nt) =>
          n.id === nt.noteId
      }
      .joinLeft(types)
      .on {
        case ((n, nt), t) =>
          nt.map(_.typeId) === t.id
      }
      .joinLeft(colors)
      .on {
        case (((n, nt), t), c) =>
          t.map(_.color) === c.id
      }
      .filter{
        case (((n, nt), t), c) =>
          Option(filterSeq.toSet)
            .filter(_.nonEmpty)
            .map(notEmptyFilter =>
              t.map(_.id.inSet(notEmptyFilter))
            )
            .getOrElse(Some(true): Rep[Option[Boolean]])
      }
      .filter{
        case (((n, nt), t), c) =>
          n.text.toLowerCase like textFilter.toLowerCase
      }
      .map {
        case (((n, nt), t), c) =>
          n
      }
      .groupBy(n => n)
      .map(_._1)

    val sortedQuery = mainQuery.sortBy {n => sortingLogic(n)}

    val pageQuery = sortedQuery
      .drop(offset)
      .take(pageSize)

    val finalQuery = pageQuery
      .joinLeft(noteTypes)
      .on {
        case (n, nt) =>
          n.id === nt.noteId
      }
      .joinLeft(types)
      .on {
        case ((n, nt), t) =>
          nt.map(_.typeId) === t.id
      }
      .joinLeft(colors)
      .on {
        case (((n, nt), t), c) =>
          t.map(_.color) === c.id
      }
      .map {
        case (((n, nt), t), c) =>
          (n, t, c.map(_.code))
      }

    val sortedPageQuery = finalQuery.sortBy {t => sortingLogic(t._1)}

    val notesMapF = for {
      result <- db.run(sortedPageQuery.result)
        .map {
        result =>
          result
              .foldLeft(mutable.LinkedHashMap[Note, Seq[(String, String)]]()){
                case (acc, (n, Some(t), Some(code))) => {
                  acc += (
                    n -> ((t.noteType, code) +: acc.getOrElse(n, Seq[(String, String)]()))
                    )
                }
                case (acc, (n, None, None)) => {
                  acc += (
                    n -> null
                  )
                }
              }
            .toSeq
      }
    } yield result

    for {
      totalRows <- db.run(mainQuery.length.result)
      sortedMap <- notesMapF
    } yield Page(sortedMap, page, offset, totalRows)
  }

  def insertNoteWithTypeAndFile(
    note: NoteWithTypes,
    fileStorageOperation: Option[Long => Future[Unit]],
    fileName: Option[String] = None
  ) = {
    val query = for {
      lastNoteId <- (
        notes
          .map(
            n => (
              n.text,
              n.color,
              n.fileName))
          returning notes.map(_.id)) += (note.text, note.color, fileName)
      _ <- note.types match {
        case Some(s) => DBIO.sequence(s.map(t => noteTypes.map(nt => (nt.noteId, nt.typeId)) += (lastNoteId, t.toLong)))
        case None => DBIO.successful(None)
      }
      _ <- fileStorageOperation.map {
        operation =>
          DBIO.from(operation(lastNoteId))
      }.getOrElse(DBIO.successful(()))

    } yield()
    db.run(query.transactionally)
  }

  def findFileNameById(noteId: Long): Future[Option[String]] = {
    val query = notes.filter(n => n.id === noteId).map(_.fileName)
    db.run(query.result.head)
  }

  def archiveNote(noteId: Long): Future[Int] = {
    val query = for {
      nrDeleted <- notes.filter(n => n.id === noteId).delete
      _ <- DBIO.seq(noteTypes.filter(nt => nt.noteId === noteId).delete)
      _ <- DBIO.from(Future(
            new File(configuration.underlying.getString("pathForUploadedFiles") + noteId.toString).delete
      ))
    } yield nrDeleted
    db.run(query.transactionally)
  }

  def findNoteTypes(noteId: Long): Future[Seq[Option[Long]]] = {
    val query = notes
      .filter(n => n.id === noteId)
      .joinLeft(noteTypes)
      .on {
        case (n, nt) =>
          n.id === nt.noteId
      }
      .joinLeft(types)
      .on {
        case ((n, nt), t) =>
          nt.map(_.typeId) === t.id
      }
      .map {
        case ((n, nt), t) =>
          t.map(_.id)
      }

    db.run(query.result)
  }

  def updateNote(
                  noteId: Long,
                  note: EditNoteForm,
                  fileUpdateOperation: Option[Long => Future[Unit]],
                  fileName: Option[String] = None
                ) = {
    val query = for {
      _ <- notes.insertOrUpdate(Note(noteId, note.text, note.color, fileName))
      _ <- DBIO.seq(
        noteTypes
          .filter(nt => nt.noteId === noteId && !note.types.contains(nt.typeId))
          delete
      )
      _ <- note.types match{
        case Some(s) => DBIO.sequence(
          s
            .map(
              tId =>
                noteTypes
                  .filter(nt => nt.noteId === noteId && nt.typeId === tId.toLong)
                  .exists
                  .result
                  .flatMap { exists =>
                    if (!exists) {
                      noteTypes.map(nt => (nt.noteId, nt.typeId)) += (noteId, tId.toLong)
                    } else {
                      DBIO.successful(None)
                    }
                  }
            )
        )
        case None => DBIO.successful(None)
      }
      _ <- fileUpdateOperation.map {
        operation =>
          DBIO.from(operation(noteId))
      }.getOrElse(DBIO.successful(()))
    } yield ()
    db.run(query.transactionally)
  }
}
