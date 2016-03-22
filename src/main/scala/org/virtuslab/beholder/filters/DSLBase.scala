package org.virtuslab.beholder.filters

import org.virtuslab.beholder.collectors.{DbCollector, Collector}
import org.virtuslab.beholder.views.BaseView
import org.virtuslab.unicorn.LongUnicornPlay.driver.api._

import scala.language.higherKinds
import scala.language.implicitConversions

abstract class DSLBase[DSLField <: FilterField, FilterType[E, ET, T] <: LightFilter[E, ET, T], FieldMapper[_]] {

  def create[E, ET, T <: BaseView[ET]](viewFilterState: ViewFilterState[E, ET, T]): FilterType[E, ET, T]
  def create[E, ET, T](viewFilterState: FilterQueryState[E, ET, T]): FilterType[E, ET, T]

  final implicit def state2Filter[E, ET, T <: BaseView[ET]](viewFilterState: ViewFilterState[E, ET, T]): FilterType[E, ET, T] =
    create(viewFilterState)

  final implicit def state2Filter[E, ET, T](tableFilterState: FilterQueryState[E, ET, T]): FilterType[E, ET, T] =
    create(tableFilterState)

  protected class TableBasedFilter[E, ET, T](state: FilterQueryState[E, ET, T]) extends LightFilter[E, ET, T] {
    override def columnFor(q: T, name: String): Option[Rep[_]] = state.columns.get(name).map(_.apply(q))

    override def baseQuery: FilterQuery = state.baseQuery

    override  def defaultOrder(q: T): Rep[_] = state.order(q)

    override  def fieldFor(name: String): Option[FilterField] = state.fields.get(name)

    override def collector: Collector[E, ET, T] = state.collector
  }

  protected class ViewBasedFilter[E, ET, T <: BaseView[ET]](state: ViewFilterState[E, ET, T]) extends LightFilter[E, ET, T] {
    override  def columnFor(q: T, name: String): Option[Rep[_]] = Option(q.columnByName(name))

    override def baseQuery: FilterQuery = state.table

    override  def defaultOrder(q: T): Rep[_] = q.id

    override  def fieldFor(name: String): Option[FilterField] = state.fields.get(name)

    override def collector: Collector[E, ET, T] = state.collector
  }

  protected case class ViewFilterState[E, ET, T <: BaseView[ET]](table: Query[T, ET, Seq],
      fields: Map[String, DSLField],
      val collector: Collector[E, ET, T]) {

    def and(name: String): AndDSL = new AndDSL(name)

    class AndDSL(name: String) {
      def asUntyped(field: DSLField): ViewFilterState[E, ET, T] =
        ViewFilterState.this.copy(fields = fields + (name -> field))

      def as[A: FieldMapper](field: MappedFilterField[A] with DSLField): ViewFilterState[E, ET, T] =
        asUntyped(field)
    }
  }

  protected case class FilterQueryState[E, ET, T](
      val baseQuery: Query[T, ET, Seq],
      val fields: Map[String, DSLField],
      val columns: Map[String, T => Rep[_]],
      val order: T => Rep[_],
      val collector: Collector[E, ET, T]) {

    def and(name: String): AndDSL = new AndDSL(name)

    def and[A: FieldMapper](name: String, col: T => Rep[A]): FilterQueryState[E, ET, T] = and(name).from(col)

    class AndDSL(name: String) {
      case class asUnsafe(field: DSLField) {
        def from(column: T => Rep[_]): FilterQueryState[E, ET, T] =
          FilterQueryState.this.copy(
            fields = fields + (name -> field),
            columns = columns + (name -> column)
          )
      }

      case class as[A: FieldMapper, B](field: MappedFilterField[A] with DSLField) {
        def from(column: T => Rep[A]): FilterQueryState[E, ET, T] =
          FilterQueryState.this.copy(
            fields = fields + (name -> field),
            columns = columns + (name -> column)
          )

        def fromOpt(column: T => Rep[Option[A]]): FilterQueryState[E, ET, T] =
          FilterQueryState.this.copy(
            fields = fields + (name -> field),
            columns = columns + (name -> column)
          )
      }

      def from[A: FieldMapper](col: T => Rep[A]): FilterQueryState[E, ET, T] =
        as(in[A]).from(col)

      /* def fromMapped[A: FieldMapper, B](inColumn: MappedFilterField[A, A] => MappedFilterField[A, B])
                                         (col: T => Rep[A]): FilterTableState[E, T] =
        as(inColumn(new IdentityField[A])).from(col)*/

    }

    def orderedBy(newOrder: T => Rep[_]): FilterQueryState[E, ET, T] =
      copy(order = newOrder)

    def mapped[NE](mapper: ET => NE): FilterQueryState[NE, ET, T] =
      copy(collector = new DbCollector(mapper))

    def withCollector[NE](collector: Collector[NE, ET, T]): FilterQueryState[NE, ET, T] =
      copy(collector = collector)

  }

  def fromTable[E, T](filter: Query[T, E, Seq])(order: T => Rep[_]) =
    new FilterQueryState[E, E, T](filter, Map(), Map(), order, new DbCollector(identity))

  def fromView[E, T <: BaseView[E]](table: Query[T, E, Seq]): ViewFilterState[E, E, T] =
    new ViewFilterState(table, Map.empty, new DbCollector(identity))

  def in[T: FieldMapper]: DSLField with MappedFilterField[T]

  def inText: DSLField with MappedFilterField[String]

  def inEnum[T <: Enumeration](implicit to: FieldMapper[T#Value]): DSLField with MappedFilterField[T#Value]
}
