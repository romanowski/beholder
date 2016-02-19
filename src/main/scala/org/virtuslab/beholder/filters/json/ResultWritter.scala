package org.virtuslab.beholder.filters.json

import org.virtuslab.beholder.filters.{FilterDefinition, FilterResult}
import play.api.libs.json._

object ResultWritter {
  import JsonKeys.Results._

  def formatResults[E](filter: JsonAwareFilter[E])(results: FilterResult[E], definition: FilterDefinition): JsValue =
    JsObject(Seq(
      filterKey -> JsonDefinitionFormatter.formatFor(filter).writes(definition),
      resultKey -> JsObject(Seq(
        dataKey -> JsArray(results.content.map(filter.writeEntry)),
        totalKey -> JsNumber(results.total)
      ))
    ))
}
