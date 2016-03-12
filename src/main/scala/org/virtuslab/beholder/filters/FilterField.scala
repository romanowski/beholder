package org.virtuslab.beholder.filters

import org.virtuslab.unicorn.LongUnicornPlay.driver.api._
import slick.ast.BaseTypedType

/**
 * filter field - there is information how read parameters from form data (mapping)
 * and how create sql's where statement(filter on column) for it
 */
trait FilterField {

  /**
   * filter on column - apply filter form data into sql - default returns true
   */
  def doFilter(column: Rep[_])(value: Any): Rep[Option[Boolean]]
}

abstract class MappedFilterField[A: BaseTypedType, B] extends FilterField {
  override final def doFilter(column: Rep[_])(value: Any): Rep[Option[Boolean]] =
    filterOnColumn(column.asInstanceOf[Rep[A]])(value.asInstanceOf[B])

  protected def filterOnColumn(column: Rep[A])(value: B): Rep[Option[Boolean]]
}

class EnumField[T <: Enumeration](implicit tm: BaseTypedType[T#Value]) extends MappedFilterField[T#Value, T#Value] {
  override protected def filterOnColumn(column: Rep[T#Value])(value: T#Value): Rep[Option[Boolean]] = column.? === value
}

class IdentityField[T: BaseTypedType] extends MappedFilterField[T, T] {
  override protected def filterOnColumn(column: Rep[T])(value: T): Rep[Option[Boolean]] = column.? === value
}

class RangeField[T: BaseTypedType] extends MappedFilterField[T, FilterRange[T]] {
  override def filterOnColumn(column: Rep[T])(value: FilterRange[T]): Rep[Option[Boolean]] = {
    value match {
      case FilterRange(Some(from), Some(to)) => column >= from && column <= to
      case FilterRange(None, Some(to)) => column <= to
      case FilterRange(Some(from), None) => column >= from
      case _ => LiteralColumn(Some(true))
    }
  }
}

class IgonredField[T: BaseTypedType] extends MappedFilterField[T, Any] {
  override protected def filterOnColumn(column: Rep[T])(value: Any): Rep[Option[Boolean]] = LiteralColumn(Some(true))
}
