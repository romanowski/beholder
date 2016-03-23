package org.virtuslab.beholder.filters

import org.virtuslab.beholder.collectors.{DbCollector, Collector}
import org.virtuslab.beholder.views.BaseView
import org.virtuslab.unicorn.LongUnicornPlay.driver.api._

import scala.language.higherKinds
import scala.language.implicitConversions

abstract class DSLBase[DSLField <: FilterField, FilterType[E, T] <: LightFilter[E, T], FieldMapper[_]] {

  def create[E,  T <: BaseView[E]](viewFilterState: ViewFilterState[E,  T]): FilterType[E, T]
  def create[E,  T](viewFilterState: FilterQueryState[E,  T]): FilterType[E, T]

  final implicit def state2Filter[E,  T <: BaseView[E]](viewFilterState: ViewFilterState[E,  T]): FilterType[E,  T] =
    create(viewFilterState)

  final implicit def state2Filter[E,  T](tableFilterState: FilterQueryState[E,  T]): FilterType[E,  T] =
    create(tableFilterState)

  protected class TableBasedFilter[E,  T](state: FilterQueryState[E,  T]) extends LightFilter[E,  T] {



    /*override def columnFor(q: T, name: String): Option[Rep[_]] = state.columns.get(name).map(_.apply(q))

    override def baseQuery: FilterQuery =

    override  def defaultOrder(q: T): Rep[_] = state.order(q)

    override  def fieldFor(name: String): Option[FilterField] = state.fields.get(name)

    override def collector: Collector[E,  T] = state.collector*/
    override def baseQuery: FilterQuery = state.baseQuery

    override protected def filterFields: Map[String, FilterField] = state.fields

    override protected def defaultOrder(from: T): Rep[_] = state.order(from)

    override protected def filterColumns(from: T): Map[String, Rep[_]] = state.columns.mapValues(_.apply(from)) //TODO rewrite this part of dsl
  }

  protected class ViewBasedFilter[E,  T <: BaseView[E]](state: ViewFilterState[E,  T]) extends LightFilter[E,  T] {
    override def baseQuery: FilterQuery = state.table

    override  def defaultOrder(q: T): Rep[_] = q.id

    override protected def filterFields: Map[String, FilterField] = state.fields

    override protected def filterColumns(from: T): Map[String, Rep[_]] =
      state.fields.map{ case (name, _) => name -> from.columnByName(name)}.toMap
  }

  protected case class ViewFilterState[E,  T <: BaseView[E]](table: Query[T, E, Seq],
      fields: Map[String, DSLField]) {

    def and(name: String): AndDSL = new AndDSL(name)

    class AndDSL(name: String) {
      def asUntyped(field: DSLField): ViewFilterState[E,  T] =
        ViewFilterState.this.copy(fields = fields + (name -> field))

      def as[A: FieldMapper](field: MappedFilterField[A] with DSLField): ViewFilterState[E,  T] =
        asUntyped(field)
    }
  }

  protected case class FilterQueryState[E,  T](
      val baseQuery: Query[T, E, Seq],
      val fields: Map[String, DSLField],
      val columns: Map[String, T => Rep[_]],
      val order: T => Rep[_]) {

    def and(name: String): AndDSL = new AndDSL(name)

    def and[A: FieldMapper](name: String, col: T => Rep[A]): FilterQueryState[E,  T] = and(name).from(col)

    class AndDSL(name: String) {
      case class asUnsafe(field: DSLField) {
        def from(column: T => Rep[_]): FilterQueryState[E,  T] =
          FilterQueryState.this.copy(
            fields = fields + (name -> field),
            columns = columns + (name -> column)
          )
      }

      case class as[A: FieldMapper, B](field: MappedFilterField[A] with DSLField) {
        def from(column: T => Rep[A]): FilterQueryState[E,  T] =
          FilterQueryState.this.copy(
            fields = fields + (name -> field),
            columns = columns + (name -> column)
          )

        def fromOpt(column: T => Rep[Option[A]]): FilterQueryState[E,  T] =
          FilterQueryState.this.copy(
            fields = fields + (name -> field),
            columns = columns + (name -> column)
          )
      }

      def from[A: FieldMapper](col: T => Rep[A]): FilterQueryState[E,  T] =
        as(in[A]).from(col)

      /* def fromMapped[A: FieldMapper, B](inColumn: MappedFilterField[A, A] => MappedFilterField[A, B])
                                         (col: T => Rep[A]): FilterTableState[E, T] =
        as(inColumn(new IdentityField[A])).from(col)*/

    }

    def orderedBy(newOrder: T => Rep[_]): FilterQueryState[E,  T] =
      copy(order = newOrder)

  }

  def fromTable[E, T](filter: Query[T, E, Seq])(order: T => Rep[_]) =
    new FilterQueryState[E, T](filter, Map(), Map(), order)

  def fromView[E, T <: BaseView[E]](table: Query[T, E, Seq]): ViewFilterState[E, T] =
    new ViewFilterState(table, Map.empty)

  def in[T: FieldMapper]: DSLField with MappedFilterField[T]

  def inText: DSLField with MappedFilterField[String]

  def inEnum[T <: Enumeration](implicit to: FieldMapper[T#Value]): DSLField with MappedFilterField[T#Value]
}
