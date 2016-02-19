package org.virtuslab.beholder.filters.json

import org.virtuslab.beholder.filters.{Order, FilterConstrains, FilterDefinition}
import play.api.libs.json._


object JsonDefinitionFormatter{

  import play.api.libs.functional.syntax._
  import play.api.libs.json._

  private implicit val orderingFormatter: Format[Order] = (
    (__ \ "column").format[String] and
      (__ \ "asc").format[Boolean]
    )(Order.apply, unlift(Order.unapply))

  def formatFor(filter: BaseJsonFilter): Format[FilterDefinition] =
      ((__ \ "take").formatNullable[Int] and
      (__ \ "skip").formatNullable[Int] and
      (__ \ "ordering").format[Seq[Order]] and
      (__ \ "data").format(new JsonConstrainsFormatter(filter)))(FilterDefinition.apply, unlift(FilterDefinition.unapply))
}


case class JsonConstrainsFormatter(filter: BaseJsonFilter) extends Format[FilterConstrains] {

  import JsonKeys.Constrains._

  override def reads(json: JsValue): JsResult[FilterConstrains] =
    readNested(json \ nestedConstrainsKey)(readFields(json \ fieldConstrainsKey))

  override def writes(o: FilterConstrains): JsValue = JsObject(Seq(
    fieldConstrainsKey -> writeFieldConstrains(o.fieldConstrains),
    nestedConstrainsKey -> writeNestedConstrains(o.nestedConstrains)
  ))

  //TODO unified both
  private def readFields(jsValue: JsValue): JsResult[FilterConstrains] = {

    val foldStart: JsResult[FilterConstrains] = JsSuccess(FilterConstrains())

    def appendNestedField(to: JsResult[FilterConstrains], name: String, json: JsValue): JsResult[FilterConstrains] ={
      filter.jsonField(name) match{
        case Some(field) =>
          val result = field.readFilter(json)

          to.flatMap { constrains =>
            result.map(constrains.addFieldConstrain(name))
          }
        case _ =>
          to.flatMap(_ => JsError(s"Field $name is not mapped in ${filter.name}"))
      }
    }

    jsValue match {
      case JsObject(values) =>
        values.foldLeft(foldStart) {
          case (current, (name, json)) =>
            appendNestedField(current, name, json)
        }
      case _ =>
        JsError(s"Nested filter definition in ${filter.name} must be js object!s")
    }


  }

  //todo too ugly!!
  private def readNested(jsValue: JsValue)(result: JsResult[FilterConstrains]): JsResult[FilterConstrains] = {
    def appendNested(to: JsResult[FilterConstrains], name: String, json: JsValue): JsResult[FilterConstrains] =
      filter.nestedFilterFor(name) match {
        case Some(filter) =>
          val nestResult = JsonConstrainsFormatter(filter).reads(json)
          to.flatMap {
            c => nestResult.map(c.addNested(name))
          }
        case None => to.flatMap(_ => JsError(s"Filter ${filter.name} hasn't got nested filter: $name"))
      }

    jsValue match {
      case JsObject(values) =>
        values.foldLeft(result) {
          case (current, (name, json)) =>
            appendNested(current, name, json)
        }
      case _ =>
        JsError(s"Nested filter definition in ${filter.name} must be js object!s")
    }
  }

  def noSuchField(name: String): JsonFilterField = throw new RuntimeException(s"No such field $name")

  private def writeFieldConstrains(constrains: Map[String, Any]) =
    JsObject(
      constrains.map {
        case (name, value) => name -> filter.jsonField(name).getOrElse(noSuchField(name)).writeFilter(value)
      }.toSeq
    )

  private def writeNestedConstrains(constrains: Map[String, FilterConstrains]): JsValue =
    JsObject(constrains.mapValues(writes).toSeq)

}