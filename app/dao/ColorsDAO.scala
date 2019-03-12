package dao

import javax.inject.Inject
import models.Color
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfig, HasDatabaseConfigProvider}
import util.MyPostgresDriver

import scala.concurrent.{ExecutionContext, Future}

trait ColorsComponent { self: HasDatabaseConfig[MyPostgresDriver] =>
  import profile.api._

  class Colors(tag: Tag) extends Table[Color](tag, "COLOR") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")
    def code = column[String]("code")

    def * = (id, name, code) <> (Color.tupled, Color.unapply _)
  }
}

class ColorsDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext) extends ColorsComponent with HasDatabaseConfigProvider[MyPostgresDriver] {
  import profile.api._

  val colors = TableQuery[Colors]

  def options(): Future[Seq[(String, String)]] = {
    val query = (for {
      color <- colors
    } yield (color.code, color.name)).sortBy(/*name*/_._2)

    db.run(query.result.map(rows => rows.map { case (code, name) => (code, name) }))
  }

  def exists(code : String) : Future[Boolean] =
    db.run(colors.filter(c => c.code === code).exists.result)

  def findIdbyCode(code: String): Future[Long] = {
    val query = colors.filter(c => c.code.toLowerCase === code.toLowerCase)
    db.run(
      query
        .result
        .filter(_.nonEmpty)
        .map(row => row.map (c => c.id).min)
    )
  }
}
