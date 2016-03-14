package org.virtuslab.beholder.filters
package json

import play.api.libs.functional.syntax._

import slick.ast.BaseTypedType
import play.api.libs.json._
import play.api.libs.json.Format

trait JsonFilterField {
  def fieldTypeDefinition: JsValue

  def readFilter(value: JsValue): JsResult[Any] //= filterFormat.reads(value)

  def writeFilter(value: Any): JsValue //= filterFormat.writes(value.asInstanceOf[B])

  def isIgnored = false
}

trait MappedJsonFilterField[A, B] extends JsonFilterField {
  protected def filterFormat: Format[B]

  override def readFilter(value: JsValue): JsResult[Any] = filterFormat.reads(value)

  override def writeFilter(value: Any): JsValue = filterFormat.writes(value.asInstanceOf[B])
}

class IdentityJsonField[A: BaseTypedType: JsonTypedType] extends IdentityField[A] with MappedJsonFilterField[A, A] {
  private val jsonTypeType = implicitly[JsonTypedType[A]]

  override protected def filterFormat: Format[A] = jsonTypeType.format

  override def fieldTypeDefinition: JsValue = jsonTypeType.fieldJsonDefinition
}

class IdentityRangeJsonField[A: BaseTypedType: JsonTypedType]
    extends RangeField[A] with MappedJsonFilterField[A, FilterRange[A]] {

  private val jsonTypeType = implicitly[JsonTypedType[A]]

  override protected def filterFormat: Format[FilterRange[A]] =
    ((__ \ "from").formatNullable(jsonTypeType.format) and
    (__ \ "to").formatNullable(jsonTypeType.format))(FilterRange.apply, unlift(FilterRange.unapply))

  override def fieldTypeDefinition: JsValue = jsonTypeType.fieldJsonDefinition
}