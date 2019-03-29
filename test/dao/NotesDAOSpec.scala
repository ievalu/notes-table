package dao

import com.dimafeng.testcontainers.PostgreSQLContainer
import models.{Note, NoteForm}
import org.scalatest.{OptionValues, TestData}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerTest
import play.api.Application
import utility.application.TestApplications.basicDatabaseTestApplication
import utility.database.PlayPostgreSQLTest

class NotesDAOSpec extends PlaySpec
  with GuiceOneAppPerTest
  with PlayPostgreSQLTest
  with ScalaFutures
  with IntegrationPatience
  with OptionValues {

  override val container: PostgreSQLContainer = PostgreSQLContainer("postgres:alpine")

  override def newAppForTest(testData: TestData): Application = {
    basicDatabaseTestApplication(container)
  }

  "NotesDAO" must {
    "create note" in {
      val dao = app.injector.instanceOf[NotesDAO]
      val text = "First note"
      val color = "#f1d6c0"
      val noteId = dao.insertNoteWithType(text).futureValue
      dao.findById(noteId).futureValue.value mustEqual Note(
        noteId,
        text,
        color,
        None
      )
    }
  }
}
