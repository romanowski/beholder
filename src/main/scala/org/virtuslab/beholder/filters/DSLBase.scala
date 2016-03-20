package org.virtuslab.beholder.filters

import org.virtuslab.beholder.views.BaseView
import org.virtuslab.unicorn.LongUnicornPlay.driver.api._

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
    override protected def columnFor(q: T, name: String): Option[Rep[_]] = state.columns.get(name).map(_.apply(q))

    override protected def table: FilterQuery = state.table

    override protected def defaultOrder(q: T): Rep[_] = state.order(q)

    override protected def fieldFor(name: String): Option[FilterField] = state.fields.get(name)
  }

  protected class ViewBasedFilter[E, T <: BaseView[E]](state: ViewFilterState[E, T]) extends LightFilter[E, T] {
    override protected def columnFor(q: T, name: String): Option[Rep[_]] = Option(q.columnByName(name))

    override protected def table: FilterQuery = state.table

    override protected def defaultOrder(q: T): Rep[_] = q.id

    override protected def fieldFor(name: String): Option[FilterField] = state.fields.get(name)
  }

  protected case class ViewFilterState[E, T <: BaseView[E]](val table: Query[T, E, Seq], val fields: Map[String, DSLField]) {

    def and(name: String): AndDSL = new AndDSL(name)

    class AndDSL(name: String) {
      def asUntyped(field: DSLField): ViewFilterState[E, T] =
        ViewFilterState(table, fields + (name -> field))


      def as[A: FieldMapper](field: MappedFilterField[A] with DSLField): ViewFilterState[E, T] =
        asUntyped(field)
    }
  }

  protected case class FilterTableState[E, T <: Table[E]](
      val table: Query[T, T#TableElementType, Seq],
      val fields: Map[String, DSLField],
      val columns: Map[String, T => Rep[_]],
      val order: T => Rep[_]) {

    def and(name: String): AndDSL = new AndDSL(name)

    def and[A: FieldMapper](name: String, col: T => Rep[A]): FilterTableState[E, T] = and(name).from(col)

    class AndDSL(name: String) {
      case class asUnsafe(field: DSLField) {
        def from(column: T => Rep[_]): FilterTableState[E, T] =
          new FilterTableState(
            table = table,
            fields = fields + (name -> field),
            columns = columns + (name -> column),
            order = order
          )
      }

      case class as[A: FieldMapper, B](field: MappedFilterField[A] with DSLField) {
        def from(column: T => Rep[A]): FilterTableState[E, T] =
          new FilterTableState(
            table = table,
            fields = fields + (name -> field),
            columns = columns + (name -> column),
            order = order
          )
      }

      def from[A: FieldMapper](col: T => Rep[A]): FilterTableState[E, T] =
        as(in[A]).from(col)

      /* def fromMapped[A: FieldMapper, B](inColumn: MappedFilterField[A, A] => MappedFilterField[A, B])
                                         (col: T => Rep[A]): FilterTableState[E, T] =
        as(inColumn(new IdentityField[A])).from(col)*/

    }

    def orderedBy(newOrder: T => Rep[_]): FilterTableState[E, T] =
      new FilterTableState(table, fields, columns, newOrder)
  }

  def fromTable[E, T <: Table[E]](filter: Query[T, E, Seq])(order: T => Rep[_]) =
    new FilterTableState[E, T](filter, Map(), Map(), order)

  def fromView[E, T <: BaseView[E]](table: Query[T, E, Seq]): ViewFilterState[E, T] =
    new ViewFilterState(table, Map.empty)

  def in[T: FieldMapper]: DSLField with MappedFilterField[T]

  def inText: DSLField with MappedFilterField[String]

  def inEnum[T <: Enumeration]: DSLField with MappedFilterField[T#Value] = ??? // TODO implement this!
}
