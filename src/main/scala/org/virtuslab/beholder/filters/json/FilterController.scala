package org.virtuslab.beholder.filters.json

import org.virtuslab.beholder.filters.FilterDefinition
import play.api.mvc._
import play.api.libs.json._
import org.virtuslab.unicorn.LongUnicornPlay.driver.simple.Session

abstract class FilterController[Entity](filter: JsonAwareFilter[Entity]) extends Controller {

  protected def inSession(body: Request[AnyContent] => Session => JsResult[JsValue]): EssentialAction

  final def filterDefinition = inSession { request =>
    _ =>
      JsSuccess(filter.jsonDefinition)
  }

  //for filter modification such us setting default parameters etc.
  protected def mapFilterData(data: FilterDefinition) = data

  final def doFilter: EssentialAction =
    inSession {
      request =>
        implicit session =>
          for {
            json <- request.body.asJson.fold[JsResult[JsValue]](JsError("not valid json"))(js => JsSuccess(js))
            filterDefinition <- JsonDefinitionFormatter.formatFor(filter).reads(json)
            data = filter.filterWithTotalEntitiesNumber(mapFilterData(filterDefinition))
          } yield ResultWritter.formatResults(filter)(data, filterDefinition)
    }
}
