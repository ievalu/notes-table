package controllers

import java.io.File
import java.nio.file.Paths
import java.util.concurrent.TimeUnit

import javax.inject.Inject
import models._
import play.api.Play
import play.api.data.Form
import play.api.data.Forms.longNumber
import play.api.data.Forms.mapping
import play.api.data.Forms.nonEmptyText
import play.api.data.Forms.optional
import play.api.data.Forms.seq
import play.api.mvc.{Action, AnyContent, Controller}
import views.html
import Play.current
import play.api.i18n.Messages.Implicits._
import dao._
import util._

import scala.Boolean
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Success

/** Manage a database of computers. */
class Application @Inject()(companiesDao:CompaniesDAO, computersDao:ComputersDAO, notesDao: NotesDAO, colorsDao: ColorsDAO, noteTypesDao: NoteTypesDAO, typesDao: TypesDAO, configuration: play.api.Configuration)(implicit ec: ExecutionContext) extends Controller {

  /** This result directly redirect to the application home.*/
  val Home = Redirect(routes.Application.list(0, 2, ""))
  val NotesHome = Redirect(routes.Application.listNotes(0, 2))
  val NoteWithTypesHome = Redirect(routes.Application.listNotesWithTypes(0, 2))

  /** Describe the computer form (used in both edit and create screens).*/
  val computerForm = Form(
    mapping(
      "id" -> optional(longNumber),
      "name" -> nonEmptyText,
      "introduced" -> optional(date),
      "discontinued" -> optional(date),
      "company" -> optional(longNumber))(Computer.apply)(Computer.unapply))

  val noteForm = Form(
    mapping(
      "id" -> longNumber,
      "text" -> nonEmptyText,
      "color" -> longNumber)(Note.apply)(Note.unapply)
  )

  val noteWithTypesForm = Form(
    mapping(
      "text" -> nonEmptyText,
      "types" -> seq(nonEmptyText))(NoteWithTypes.apply)(NoteWithTypes.unapply)
  )

  // -- Actions

  /** Handle default path requests, redirect to computers list */
  def index = Action { Home }

  /** Display the paginated list of computers.
    *
    * @param page Current page number (starts from 0)
    * @param orderBy Column to be sorted
    * @param filter Filter applied on computer names
    */
  def list(page: Int, orderBy: Int, filter: String) = Action.async { implicit request =>
    val computers = computersDao.list(page = page, orderBy = orderBy, filter = ("%" + filter + "%"))
    computers.map(cs => Ok(html.list(cs, orderBy, filter)))
  }

  /** Display the 'edit form' of a existing Computer.
    *
    * @param id Id of the computer to edit
    */
  def edit(id: Long) = Action.async { implicit rs =>
    val computerAndOptions = for {
      computer <- computersDao.findById(id)
      options <- companiesDao.options()
    } yield (computer, options)

    computerAndOptions.map {
      case (computer, options) =>
        computer match {
          case Some(c) => Ok(html.editForm(id, computerForm.fill(c), options))
          case None => NotFound
        }
    }
  }

  /** Handle the 'edit form' submission
    *
    * @param id Id of the computer to edit
    */
  def update(id: Long) = Action.async { implicit rs =>
    computerForm.bindFromRequest.fold(
      formWithErrors => companiesDao.options().map(options => BadRequest(html.editForm(id, formWithErrors, options))),
      computer => {
        for {
          _ <- computersDao.update(id, computer)
        } yield Home.flashing("success" -> "Computer %s has been updated".format(computer.name))
      }
    )
  }

  /** Display the 'new computer form'. */
  def create = Action.async { implicit rs =>
    companiesDao.options().map(options => Ok(html.createForm(computerForm, options)))
  }

  /** Handle the 'new computer form' submission. */
  def save = Action.async { implicit rs =>
    computerForm.bindFromRequest.fold(
      formWithErrors => companiesDao.options().map(options => BadRequest(html.createForm(formWithErrors, options))),
      computer => {
        for {
          _ <- computersDao.insert(computer)
        } yield Home.flashing("success" -> "Computer %s has been created".format(computer.name))
      }
    )
  }

  /** Handle computer deletion. */
  def delete(id: Long) = Action.async { implicit rs =>
     for {
          _ <- computersDao.delete(id)
     } yield Home.flashing("success" -> "Computer has been deleted")
  }

  def listNotes(page: Int, orderBy: Int) = Action.async {
    implicit request =>
      val notes = notesDao.list(page = page, orderBy = orderBy)
      val result = notes.map(note => Ok(html.notesList(note, orderBy)))
      result
  }

  def listNotesWithTypes(page: Int, orderBy: Int, filter: List[Long], textFilter: String) = Action.async {
    implicit request =>
      val notes = notesDao.findNotesWithTypes(page = page, orderBy = orderBy, filterSeq = filter, textFilter = ("%" + textFilter + "%"))
      // val test = Await.result(notesDao.findNotesWithTypes(page = page, orderBy = orderBy, filterSeq = filter, textFilter = textFilter), Duration(1, TimeUnit.SECONDS))
      val typeOptions = typesDao.options()
      val noteOption = for {
        note <- notes
        typeOption <- typeOptions
      } yield (note, typeOption)
      val result = noteOption.map( t => Ok(html.noteTypesList(t._1, noteWithTypesForm, t._2, orderBy, filter, textFilter)))
      result
  }

  def createNote:Action[AnyContent] = Action.async { implicit rs =>
    colorsDao.options().map(options => Ok(html.createNoteForm(noteForm, options)))
  }

  def createNoteWithType:Action[AnyContent] = Action.async { implicit rs =>
    typesDao.options().map(options => Ok(html.createNoteWithTypeForm(noteWithTypesForm, options)))
  }

  def saveNote = Action.async {
    implicit request =>
      noteForm.bindFromRequest.fold(
        formWithErrors => colorsDao.options().map(options => BadRequest(html.createNoteForm(formWithErrors, options))),
        note => {
          for {
            _ <- notesDao.insert(note)
          } yield NotesHome.flashing("success" -> "Note has been created")
        }
      )
  }

  def saveNoteWithType = Action.async(parse.multipartFormData) {
    implicit request =>
      /*val filledForm = noteWithTypesForm.bindFromRequest()

      if (filledForm.hasErrors) {
        typesDao.options().map(options => BadRequest(html.createNoteWithTypeForm(filledForm, options)))
      }  else {
        val resource = filledForm.get
        val file = request.body.file("noteFile")
          .map{ noteFile =>
            val filename = Paths.get(noteFile.filename).getFileName
            //val fileSize = noteFile.fileSize No such method!?
            val contentType = noteFile.contentType

            noteFile.ref.moveTo(Paths.get(configuration.underlying.getString("pathForUploadedFiles")), true)
            for {
              lastNoteId <- notesDao.insertNoteWithType(filledForm.  .text)
              _ <- noteTypesDao.insert(lastNoteId, filledForm.types.map(value => value.toLong))
            } yield NoteWithTypesHome.flashing("success" -> "Note has been created")
          }
      }*/
      noteWithTypesForm.bindFromRequest.fold(
        formWithErrors => typesDao.options().map(options => BadRequest(html.createNoteWithTypeForm(formWithErrors, options))),
        note => {
          val file = request.body.file("noteFile")
          file.filter(_.ref.path.toFile.length() > 0).map { noteFile =>
            val filename = Paths.get(noteFile.filename).getFileName
            //val fileSize = noteFile.fileSize No such method!?
            val contentType = noteFile.contentType

            noteFile.ref.moveTo(Paths.get(configuration.underlying.getString("pathForUploadedFiles") + filename), true)
            for {
              lastNoteId <- notesDao.insertNoteWithType(note.text)
              _ <- noteTypesDao.insert(lastNoteId, note.types.map(value => value.toLong))
            } yield NoteWithTypesHome.flashing("success" -> "Note has been created")
          }.getOrElse(Future(Redirect(routes.Application.createNoteWithType()).flashing("error" -> "Missing file")))
        }
      )
  }
}
