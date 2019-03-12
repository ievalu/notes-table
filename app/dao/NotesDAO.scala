package dao

import java.io.File
import java.nio.file.{Files, Path, Paths}

import javax.inject.{Inject, Singleton}
import models._
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfig, HasDatabaseConfigProvider}
import play.api.libs.Files
import play.api.libs.Files.TemporaryFile
import play.api.mvc.MultipartFormData.FilePart
import play.mvc.BodyParser.MultipartFormData
import util.MyPostgresDriver

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.blocking

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
          nt.flatMap(_.typeId) === t.id
      }
      .joinLeft(colors)
      .on {
        case (((n, nt), t), c) =>
          t.flatMap(_.color) === c.id
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

  def list(page: Int = 0, pageSize: Int = 10, orderBy: Int = 1): Future[Page[Note]] = {

    val offset = pageSize * page
    val query =
      notes
        .drop(offset)
        .take(pageSize)

    for {
      totalRows <- count()
      result <- db.run(query.result)
    } yield Page(result, page, offset, totalRows)
  }

  def getNotes(page: Int = 0, pageSize: Int = 10, orderBy: Int = 1): Future[Seq[Note]] = {
    val offset = pageSize * page
    db.run {
      notes.drop(offset).take(pageSize).result
    }
  }

  def findNotesWithTypes(page: Int = 0, pageSize: Int = 10, orderBy: Int = 1, filterSeq: List[Long], textFilter: String = "%"):Future[Page[(Note, Seq[(String, String)])]] = {
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
          t.flatMap(_.color) === c.id
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
      .distinct

    val finalQuery = mainQuery
      .drop(offset)
      .take(pageSize)
      .joinLeft(noteTypes)
      .on {
        case (n, nt) =>
          n.id === nt.noteId
      }
      .joinLeft(types)
      .on {
        case ((n, nt), t) =>
          nt.flatMap(_.typeId) === t.id
      }
      .joinLeft(colors)
      .on {
        case (((n, nt), t), c) =>
          t.flatMap(_.color) === c.id
      }
      .map {
        case (((n, nt), t), c) =>
          (n, t, c.flatMap(_.code))
      }

    for {
      totalRows <- db.run(mainQuery.length.result)
      result <- db.run(finalQuery.result)
        .map {
        result =>
          result
            .groupBy(_._1).mapValues(_.map{
            case (n, Some(t), Some(code)) => (t.noteType, code)
            case (n, None, None) => null
          })
            .toSeq
      }
    } yield Page(result, page, offset, totalRows)
  }

  def insert(note: NoteForm): Future[Int] =
    db.run(notes.map(n => (n.text, n.color)) += (note.text, note.color))

  def insertNoteWithType(noteText: String): Future[Long] =
    db.run((notes.map(n => (n.text, n.color)) returning notes.map(_.id)) += (noteText, "#f1d6c0"))
    // db.run(notes.map(_ => (noteText, 1)) += (noteText, 1))

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
          returning notes.map(_.id)) += (note.text, "#f1d6c0", fileName)
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
          t.flatMap(_.id)
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
      _ <- notes.insertOrUpdate(Note(noteId, note.text, "#f1d6c0", fileName))
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
