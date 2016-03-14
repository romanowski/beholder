package org.virtuslab.beholder.filters.json

import org.joda.time.{LocalDate, DateTime}
import org.virtuslab.beholder.filters._
import org.virtuslab.beholder.views.BaseView
import org.virtuslab.unicorn.LongUnicornPlay.driver.api._
import play.api.libs.json.{JsString, JsValue, Format}

import slick.ast.BaseTypedType

import scala.reflect.ClassTag

case class JsonTypedType[A](fieldJsonDefinition: JsValue)(implicit val typed: BaseTypedType[A], implicit val format: Format[A])

object JsonTypedType{
  def apply[A: BaseTypedType: Format](name: String): JsonTypedType[A] = JsonTypedType(JsString(name))

  def apply[A: BaseTypedType: Format: ClassTag]: JsonTypedType[A] =
    apply(implicitly[ClassTag[A]].runtimeClass.getSimpleName)

}

object JsonDSL
  extends DSLBase[FilterField
  with JsonFilterField, JsonFilterImpl, JsonTypedType]
  with JsonTypedTypeImplicits {

  override def create[E, T <: Table[E]](viewFilterState: JsonDSL.FilterTableState[E, T]): JsonFilterImpl[E, T] =
    new TableBasedFilter(viewFilterState) with JsonFilterImpl[E, T] {
      override protected def fields: Map[String, JsonFilterField] = viewFilterState.fields
    }

  override def create[E, T <: BaseView[E]](viewFilterState: JsonDSL.ViewFilterState[E, T]): JsonFilterImpl[E, T] =
    new ViewBasedFilter[E, T](viewFilterState) with JsonFilterImpl[E, T] {
      override protected def fields: Map[String, JsonFilterField] = viewFilterState.fields
    }

  override def in[T: JsonTypedType]: FilterField with JsonFilterField with MappedFilterField[T, T] = {
    val formatter: JsonTypedType[T] = implicitly
    import formatter._ //TODO report to Jetbrians
    new IdentityJsonField[T]
  }

  override def inRange[T: JsonTypedType]: FilterField with JsonFilterField with MappedFilterField[T, FilterRange[T]] = {
    val formatter: JsonTypedType[T] = implicitly
    import formatter._ //TODO report to Jetbrians
    new IdentityRangeJsonField[T]
  }

  override def inText: FilterField with JsonFilterField with MappedFilterField[String, String] = in[String]
}

trait JsonTypedTypeImplicits{
  implicit val intField = JsonTypedType[Int]
  implicit val stringField = JsonTypedType[String]
  implicit val bigDecimalField = JsonTypedType[BigDecimal]
  implicit val booleanField = JsonTypedType[Boolean]
  implicit val dateTime = JsonTypedType[DateTime]
  implicit val localDate = JsonTypedType[LocalDate]
}
