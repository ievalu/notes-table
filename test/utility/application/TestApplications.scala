package utility.application

import com.dimafeng.testcontainers.PostgreSQLContainer
import org.scalatest.mockito.MockitoSugar
import play.api.{Application, Configuration, Mode}
import play.api.inject.guice.GuiceApplicationBuilder

object TestApplications extends MockitoSugar {

  def basicDatabaseTestApplication(
      container: PostgreSQLContainer,
      evolutionsEnabled: Boolean = true
  ): Application = {
    val configuration: Configuration = Configuration.from(
      Seq(
        Some("slick.dbs.default.profile" -> "util.MyPostgresDriver$"),
        Some("slick.dbs.default.db.url" -> container.jdbcUrl),
        Some("slick.dbs.default.db.user" -> container.username),
        Some("slick.dbs.default.db.password" -> container.password),
        if (!evolutionsEnabled) {
          Some("play.evolutions.db.default.enabled" -> "false")
        } else {
          None
        }
      ).flatten.toMap
    )

    GuiceApplicationBuilder(configuration = configuration)
      .in(Mode.Test)
      .build()
  }
}
