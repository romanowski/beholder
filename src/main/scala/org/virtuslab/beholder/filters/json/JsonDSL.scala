package org.virtuslab.beholder.filters.json

import org.virtuslab.beholder.filters._
import org.virtuslab.beholder.views.BaseView
import org.virtuslab.unicorn.LongUnicornPlay
import org.virtuslab.unicorn.LongUnicornPlay.driver.simple._
import play.api.libs.json.Format

import scala.slick.ast.{ BaseTypedType, TypedType }

class JsonTypedType[A](implicit val typed: BaseTypedType[A], implicit val format: Format[A])

object JsonDSL extends DSLBase[FilterField with JsonFilterField, JsonFilterImpl, JsonTypedType] {
  override def create[E, T <: Table[E]](viewFilterState: JsonDSL.FilterTableState[E, T]): JsonFilterImpl[E, T] =
    new TableBasedFilter(viewFilterState) with JsonFilterImpl[E, T] {
      override protected def fields: Map[String, JsonFilterField] = viewFilterState.fields
    }

  override def create[E, T <: BaseView[E]](viewFilterState: JsonDSL.ViewFilterState[E, T]): JsonFilterImpl[E, T] =
    new ViewBasedFilter[E, T](viewFilterState) with JsonFilterImpl[E, T] {
      override protected def fields: Map[String, JsonFilterField] = viewFilterState.fields
    }

  implicit def jsonTypedType[A: BaseTypedType: Format]: JsonTypedType[A] = {
    new JsonTypedType[A]
  }

  override def in[T: JsonTypedType]: FilterField with JsonFilterField with MappedFilterField[T, T] = {
    val formatter: JsonTypedType[T] = implicitly
    import formatter._
    new IdentityJsonField[T]
  }

  override def inRange[T: JsonTypedType]: FilterField with JsonFilterField with MappedFilterField[T, FilterRange[T]] = {
    val formatter: JsonTypedType[T] = implicitly
    import formatter._
    new IdentityRangeJsonField[T]
  }

  override def inText: FilterField with JsonFilterField with MappedFilterField[String, String] = in[String]
}
