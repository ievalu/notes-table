package dao

import javax.inject.{Inject, Singleton}
import models._
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfig, HasDatabaseConfigProvider}
import util.MyPostgresDriver

import scala.concurrent.{ExecutionContext, Future}

trait NotesComponent { self: HasDatabaseConfig[MyPostgresDriver] =>
  import profile.api._

  class Notes(tag: Tag) extends Table[Note](tag, "NOTE") {

    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def text = column[String]("text")
    def color = column[Long]("color")

    def * = (id, text, color) <> (Note.tupled, Note.unapply _)
  }
}

@Singleton
class NotesDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext)
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

  def list(page: Int = 0, pageSize: Int = 10, orderBy: Int = 1): Future[Page[(Note, Color)]] = {

    val offset = pageSize * page
    val query =
      (for {
        (note, color) <- notes joinLeft colors on (_.color === _.id)
      } yield (note, color.map(_.id), color.map(_.name), color.map(_.code)))
        .drop(offset)
        .take(pageSize)

    for {
      totalRows <- count()
      list = query.result.map { rows => rows.collect { case (note, id, Some(name), Some(code)) => (note, Color(id.get, name, code)) } }
      result <- db.run(list)
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
          nt.flatMap(_.typeId) === t.id
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

  def insert(note: Note): Future[Int] =
    db.run(notes.map(n => (n.text, n.color)) += (note.text, note.color))

  def insertNoteWithType(noteText: String): Future[Long] =
    db.run((notes.map(n => (n.text, n.color)) returning notes.map(_.id)) += (noteText, 1))
    // db.run(notes.map(_ => (noteText, 1)) += (noteText, 1))
}
