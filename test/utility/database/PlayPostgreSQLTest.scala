package utility.database

import com.dimafeng.testcontainers.{ForEachTestContainer, PostgreSQLContainer}
import org.scalatest.Suite
import org.scalatest.compatible.Assertion
import org.scalatestplus.play.AppProvider
import play.api.db.DBApi
import play.api.db.evolutions.Evolutions

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

trait PlayPostgreSQLTest extends Suite with AppProvider with ForEachTestContainer {

  override val container: PostgreSQLContainer

  def applyEvolutions(): Unit = {
    lazy val databaseApi = app.injector.instanceOf[DBApi]
    val database = databaseApi.database("default")
    try {
      Evolutions.applyEvolutions(database)
      database.shutdown()
    } catch {
      case NonFatal(e) =>
        database.shutdown()
        throw e
    }
  }

  def unapplyEvolutions(): Unit = {
    lazy val databaseApi = app.injector.instanceOf[DBApi]
    val database = databaseApi.database("default")
    try {
      Evolutions.cleanupEvolutions(database)
      database.shutdown()
    } catch {
      case NonFatal(e) =>
        database.shutdown()
        throw e
    }
  }

  def withEvolutions(
      assertionFun: () => Assertion
  ): Assertion = {
    applyEvolutions()
    val assertion = assertionFun.apply()
    unapplyEvolutions()
    assertion
  }

  def withEvolutions(
      futureAssertionFun: () => Future[Assertion]
  )(implicit ec: ExecutionContext): Future[Assertion] = {
    applyEvolutions()
    futureAssertionFun
      .apply()
      .map {
        assertion =>
          unapplyEvolutions()
          assertion
      }
  }
}
