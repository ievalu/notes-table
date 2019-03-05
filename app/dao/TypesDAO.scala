package dao

import javax.inject.Inject
import models.Type
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfig, HasDatabaseConfigProvider}
import util.MyPostgresDriver

import scala.concurrent.{ExecutionContext, Future}

trait TypesComponent { self: HasDatabaseConfig[MyPostgresDriver] =>
  import profile.api._

  class Types(tag: Tag) extends Table[Type](tag, "NTYPE") {

    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def noteType = column[String]("type")
    def color = column[Long]("color")

    def * = (id, noteType, color) <> (Type.tupled, Type.unapply _)
  }
}

class TypesDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext)
  extends ColorsComponent with TypesComponent with HasDatabaseConfigProvider[MyPostgresDriver] {
  import profile.api._

  private val noteTypes = TableQuery[Types]
  private val colors = TableQuery[Colors]

  def options(): Future[Seq[(String, String, String)]] = {
    val query = noteTypes
      .joinLeft(colors)
      .on {
        case (nt, c) =>
          nt.color === c.id
      }
        .map {
          case (nt, c) =>
            (nt.id, nt.noteType, c.flatMap(_.code))
        }
    db.run(query.result.map(rows => rows.map { case (id, noteType, noteColor) => (id.toString, noteType, noteColor.get) }))
  }
}
