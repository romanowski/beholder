package org.virtuslab.beholder.filters.json

import org.virtuslab.beholder.filters.{ FilterRange, RangeField, IdentityField, MappedFilterField }
import org.virtuslab.unicorn.LongUnicornPlay
import org.virtuslab.unicorn.LongUnicornPlay.driver.simple._
import play.api.libs.functional.syntax._

import scala.reflect.ClassTag
import scala.slick.ast.{ BaseTypedType, TypedType }
import play.api.libs.json._

trait JsonFilterField {
  def fieldTypeDefinition: JsValue

  def readFilter(value: JsValue): JsResult[Any] //= filterFormat.reads(value)

  def writeFilter(value: Any): JsValue //= filterFormat.writes(value.asInstanceOf[B])

  def isIgnored = false
}

trait MappedJsonFilterField[A, B] extends JsonFilterField {
  protected def filterFormat: Format[B]

  protected def valueWrite: Writes[A]

  override def readFilter(value: JsValue): JsResult[Any] = filterFormat.reads(value)

  override def writeFilter(value: Any): JsValue = filterFormat.writes(value.asInstanceOf[B])
}

class IdentityJsonField[A: BaseTypedType: Format] extends IdentityField[A] with MappedJsonFilterField[A, A] {
  override protected def filterFormat: Format[A] = implicitly[Format[A]]

  override protected def valueWrite: Writes[A] = implicitly[Format[A]]

  override def fieldTypeDefinition: JsValue = ??? //JsString(classOf[A].getSimpleName)
}

class IdentityRangeJsonField[A: BaseTypedType: Format]
    extends RangeField[A] with MappedJsonFilterField[A, FilterRange[A]] {

  private implicit def rangeFormat[T: Format]: Format[FilterRange[T]] =
    ((__ \ "from").format[Option[T]] and
      (__ \ "to").format[Option[T]])(FilterRange.apply, unlift(FilterRange.unapply))

  override protected def filterFormat: Format[FilterRange[A]] = implicitly[Format[FilterRange[A]]]

  override protected def valueWrite: Writes[A] = implicitly[Format[A]]

  override def fieldTypeDefinition: JsValue = ??? //JsString(classOf[A].getSimpleName)
}