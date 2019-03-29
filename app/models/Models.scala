package models

import java.time.LocalDate

import play.api.libs.json.JsValue
import play.api.mvc.QueryStringBindable

import scala.util.{Failure, Success, Try}

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

case class EditNoteForm (text: String, types: Option[Seq[String]], color: String, deleteFile: Boolean)

case class NoteWithTypes (text: String, types: Option[Seq[String]], color: String)

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

object SortableFields extends Enumeration {
  type Field = Value
  val text = Value("text")
  val file = Value("file")

  implicit def queryStringBinder(implicit stringBinder: QueryStringBindable[String]) =
    new QueryStringBindable[SortableFields.Value] {
      override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, Field]] = {
        stringBinder.bind(key, params)
          .map {
            case Right(s) =>
              Try(SortableFields.withName(s)) match {
                case Success(sortField) =>
                  Right(sortField)
                case Failure(_) =>
                  Left(s"Failed to parse sort field from '$s'")
              }
          }
      }
      override def unbind(key: String, sortField: Field): String = {
          stringBinder.unbind(key, sortField.toString)
      }
    }
}

object SortOrder extends Enumeration {
  type Order = Value
  val asc = Value("asc")
  val desc = Value("desc")

  implicit def queryStringBinder(implicit stringBinder: QueryStringBindable[String]) = new QueryStringBindable[Order] {
    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, Order]] = {
      stringBinder.bind(key, params)
        .map {
          case Right(s) =>
            Try(SortOrder.withName(s)) match {
              case Success(sortOrder) =>
                Right(sortOrder)
              case Failure(_) =>
                Left(s"Failed to parse sort order from '$s'")
            }
        }
    }

    override def unbind(key: String, sortOrder: Order): String = {
      stringBinder.unbind(key, sortOrder.toString)
    }
  }
}
