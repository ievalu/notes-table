package models

import java.time.LocalDate
import play.api.libs.json.JsValue

case class Page[A](items: Seq[A], page: Int, offset: Long, total: Long) {
  lazy val prev = Option(page - 1).filter(_ >= 0)
  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)
}

case class Company(
  id: Option[Long],
  name: String,
  props: JsValue)

case class Computer(
  id: Option[Long] = None,
  name: String,
  introduced: Option[LocalDate] = None,
  discontinued: Option[LocalDate] = None,
  companyId: Option[Long] = None)

case class Note (id: Long, text: String, color: String, fileName: Option[String])

case class NoteForm (text: String, color: String)

case class EditNoteForm (text: String, types: Option[Seq[String]], deleteFile: Boolean)

case class NoteWithTypes (text: String, types: Option[Seq[String]])

case class Color(
  id: Long,
  text: String,
  value: String
)

case class Type (
  id: Long,
  noteType: String,
  color: Long
)

case class NoteType (id: Long, noteId: Long, typeId: Long)
