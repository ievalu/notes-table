package dao

import javax.inject.Inject
import models.NoteType
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfig, HasDatabaseConfigProvider}
import util.MyPostgresDriver

import scala.concurrent.{ExecutionContext, Future}

trait NoteTypeComponent { self: HasDatabaseConfig[MyPostgresDriver] =>
  import profile.api._

  class NoteTypes(tag: Tag) extends Table[NoteType](tag, "NOTETYPE") {

    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def noteId = column[Long]("noteId")
    def typeId = column[Long]("typeId")

    def * = (id, noteId, typeId) <> (NoteType.tupled, NoteType.unapply _)

   /* def noteFK = foreignKey("FK_NOTES", noteId.get, notes)(note => note.id, onDelete=ForeignKeyAction.Cascade)

    def typeFK = foreignKey("FK_TYPES", typeId, types)(typeN => typeN.id)*/
  }
}

class NoteTypesDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext)
  extends NotesComponent with TypesComponent with NoteTypeComponent with HasDatabaseConfigProvider[MyPostgresDriver] {
  import profile.api._

  private val noteTypes = TableQuery[NoteTypes]

  def insert(noteId: Long, typeIdSeq: Seq[Long]): Future[Seq[Int]] =
    Future.sequence(typeIdSeq.map(typeId => db.run(noteTypes.map(nt => (nt.noteId, nt.typeId)) += (noteId, typeId))))
}
