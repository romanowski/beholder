package org.virtuslab.beholder.filters
package json

import org.virtuslab.unicorn.LongUnicornPlay
import play.api.libs.json._
import org.virtuslab.unicorn.LongUnicornPlay.driver.api._

trait JsonAwareFilter[E] extends JsonFilter with FilterAPI[E]

trait JsonFilter {

  protected def nested: Map[String, JsonFilter]
  protected def fields: Map[String, JsonFilterField]

  def labelFor(name: String): String = name

  def name: String

  def jsonDefinition: JsValue = JsArray(fields.map(Function.tupled(jsonFieldDefinition)).toSeq)

  private def jsonFieldDefinition(name: String, field: JsonFilterField): JsObject = JsObject(Seq(
    "key" -> JsString(name),
    "label" -> JsString(labelFor(name)),
    "type" -> field.fieldTypeDefinition,
    "nested" -> JsArray(nested.keys.map(JsString.apply).toSeq)
  ))

  def jsonField(name: String): Option[JsonFilterField] = fields.get(name)
  def nestedFilterFor(name: String): Option[JsonFilter] = nested.get(name)
}

trait JsonFilterImpl[E, TE, T] extends LightFilter[E, TE, T] with JsonAwareFilter[E] {
  override def name: String = "ala" // TODO baseQuery.shaped.value.tableName // Can be override

  private var _nested: Map[String, JsonFilter] = Map()

  override protected def nested: Map[String, JsonFilter] = _nested

  override def join[NET, NT](name: String, from: LightFilter[_, NET, NT])(on: (T, NT) => Rep[Boolean])(implicit t1Shape: Shape[FlatShapeLevel, T, TE, T]): LightFilter[E, TE, T] = {
    from match {
      case jsonFilter: JsonFilter =>
        _nested = _nested + (name -> jsonFilter)
      case _ =>
    }

    super.join(name, from)(on)
  }
}