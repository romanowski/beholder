package org.virtuslab.beholder.filters

import org.virtuslab.beholder.views.BaseView
import org.virtuslab.unicorn.LongUnicornPlay.driver.simple._

import scala.slick.ast.BaseTypedType

import scala.language.higherKinds
import scala.language.implicitConversions

abstract class DSLBase[DSLField <: FilterField, FilterType[E, T <: Table[E]] <: LightFilter[E, T], FieldMapper[_]] {

  def create[E, T <: BaseView[E]](viewFilterState: ViewFilterState[E, T]): FilterType[E, T]
  def create[E, T <: Table[E]](viewFilterState: FilterTableState[E, T]): FilterType[E, T]

  final implicit def state2Filter[E, T <: BaseView[E]](viewFilterState: ViewFilterState[E, T]): FilterType[E, T] =
    create(viewFilterState)

  final implicit def state2Filter[E, T <: Table[E]](tableFilterState: FilterTableState[E, T]): FilterType[E, T] =
    create(tableFilterState)

  protected class TableBasedFilter[E, T <: Table[E]](state: FilterTableState[E, T]) extends LightFilter[E, T] {
    override protected def columnFor(q: T, name: String): Option[Column[_]] = state.columns.get(name).map(_.apply(q))

    override protected def table: FilterQuery = state.table

    override protected def defaultOrder(q: T): Column[_] = state.order(q)

    override protected def fieldFor(name: String): Option[FilterField] = state.fields.get(name)
  }

  protected class ViewBasedFilter[E, T <: BaseView[E]](state: ViewFilterState[E, T]) extends LightFilter[E, T] {
    override protected def columnFor(q: T, name: String): Option[Column[_]] = Option(q.columnByName(name))

    override protected def table: FilterQuery = state.table

    override protected def defaultOrder(q: T): Column[_] = q.id

    override protected def fieldFor(name: String): Option[FilterField] = state.fields.get(name)
  }

  protected case class ViewFilterState[E, T <: BaseView[E]](val table: Query[T, E, Seq], val fields: Map[String, DSLField]) {

    def and(name: String): AndDSL = new AndDSL(name)

    class AndDSL(name: String) {
      def as(field: DSLField): ViewFilterState[E, T] =
        ViewFilterState(table, fields + (name -> field))
    }
  }

  protected case class FilterTableState[E, T <: Table[E]](
      val table: Query[T, T#TableElementType, Seq],
      val fields: Map[String, DSLField],
      val columns: Map[String, T => Column[_]],
      val order: T => Column[_]
  ) {

    def and(name: String): AndDSL = new AndDSL(name)

    def and[A: FieldMapper](name: String, col: T => Column[A]): FilterTableState[E, T] = and(name).from(col)

    class AndDSL(name: String) {
      case class asUnsafe(field: DSLField) {
        def from(column: T => Column[_]): FilterTableState[E, T] =
          new FilterTableState(
            table = table,
            fields = fields + (name -> field),
            columns = columns + (name -> column),
            order = order
          )
      }

      case class as[A: FieldMapper, B](field: MappedFilterField[A, B] with DSLField) {
        def from(column: T => Column[A]): FilterTableState[E, T] =
          new FilterTableState(
            table = table,
            fields = fields + (name -> field),
            columns = columns + (name -> column),
            order = order
          )
      }

      def from[A: FieldMapper](col: T => Column[A]): FilterTableState[E, T] =
        as(in[A]).from(col)

      /* def fromMapped[A: FieldMapper, B](inColumn: MappedFilterField[A, A] => MappedFilterField[A, B])
                                         (col: T => Column[A]): FilterTableState[E, T] =
        as(inColumn(new IdentityField[A])).from(col)*/

    }

    def orderedBy(newOrder: T => Column[_]): FilterTableState[E, T] =
      new FilterTableState(table, fields, columns, newOrder)
  }

  def fromTable[E, T <: Table[E]](filter: Query[T, E, Seq])(order: T => Column[_]) =
    new FilterTableState[E, T](filter, Map(), Map(), order)

  def fromView[E, T <: BaseView[E]](table: Query[T, E, Seq]): ViewFilterState[E, T] =
    new ViewFilterState(table, Map.empty)

  def in[T: FieldMapper]: DSLField with MappedFilterField[T, T]

  def inText: DSLField with MappedFilterField[String, String]

  def inRange[T: FieldMapper]: DSLField with MappedFilterField[T, FilterRange[T]]
}
