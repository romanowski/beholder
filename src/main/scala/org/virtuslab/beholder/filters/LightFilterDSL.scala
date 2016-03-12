package org.virtuslab.beholder.filters

import org.virtuslab.beholder.utils.ILikeExtension._
import org.virtuslab.beholder.views.BaseView
import org.virtuslab.unicorn.LongUnicornPlay.driver.simple._

import scala.slick.ast.BaseTypedType

object LightDSLFilter extends DSLBase[FilterField, LightFilter, BaseTypedType] {
  override def create[E, T <: BaseView[E]](viewFilterState: LightDSLFilter.ViewFilterState[E, T]): LightFilter[E, T] =
    new ViewBasedFilter[E, T](viewFilterState)

  override def create[E, T <: Table[E]](viewFilterState: LightDSLFilter.FilterTableState[E, T]): LightFilter[E, T] =
    new TableBasedFilter[E, T](viewFilterState)

  override def in[T: BaseTypedType]: FilterField with MappedFilterField[T, T] = new IdentityField[T]

  override val inText = new MappedFilterField[String, String] {
    override def filterOnColumn(column: Column[String])(data: String): Column[Option[Boolean]] = column ilike s"%${escape(data)}%"
  }

  override def inRange[T: BaseTypedType]: FilterField with MappedFilterField[T, FilterRange[T]] = new RangeField[T]
}
