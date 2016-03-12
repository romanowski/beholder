package org.virtuslab.beholder.filters
package json

import play.api.libs.json._
import org.virtuslab.unicorn.LongUnicornPlay.driver.simple._

trait JsonFilterImpl[E, T <: Table[E]] extends LightFilter[E, T] with JsonAwareFilter[E] {
  override def name: String = table.shaped.value.tableName // Can be override

  private var _nested: Map[String, BaseJsonFilter] = Map()

  override protected def nested: Map[String, BaseJsonFilter] = _nested

  override def join[TE, TT <: Table[TE]](name: String, from: LightFilter[TE, TT])(on: (T, TT) => Column[Boolean]): LightFilter[E, T] = {
    from match {
      case jsonFilter: BaseJsonFilter =>
        _nested = _nested + (name -> jsonFilter)
      case _ =>
    }

    super.join(name, from)(on)
  }
}

trait JsonAwareFilter[E] extends BaseJsonFilter with FilterAPI[E]

trait BaseJsonFilter {

  protected def nested: Map[String, BaseJsonFilter]
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
  def nestedFilterFor(name: String): Option[BaseJsonFilter] = nested.get(name)
}
