package org.virtuslab.beholder.filters.json

import org.virtuslab.beholder.filters.{FilterAPI, FilterResult, FilterDefinition, FilterField}
import play.api.libs.json._


trait JsonAwareFilter[E] extends BaseJsonFilter with FilterAPI[E]{
  def writeEntry(e: E): JsValue
}

trait BaseJsonFilter {

  protected def nested: Map[String, BaseJsonFilter]
  protected def fields: Map[String, JsonFilterField]

  def labelFor(name: String): String

  def name: String

  def jsonDefinition: JsValue =  JsArray(fields.map(Function.tupled(jsonFieldDefinition)).toSeq)

  private def jsonFieldDefinition(name: String, field: JsonFilterField): JsObject = JsObject(Seq(
    "key" -> JsString(name),
    "label" -> JsString(labelFor(name)),
    "type" -> field.fieldTypeDefinition,
    "nested" -> JsArray(nested.keys.map(JsString.apply).toSeq)
  ))

  def jsonField(name: String): Option[JsonFilterField] = fields.get(name)
  def nestedFilterFor(name: String): Option[BaseJsonFilter] = nested.get(name)
}
