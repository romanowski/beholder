package org.virtuslab.beholder.filters

import org.virtuslab.beholder.collectors.Collector
import org.virtuslab.beholder.views.BaseView
import org.virtuslab.unicorn.LongUnicornPlay._
import org.virtuslab.unicorn.LongUnicornPlay.driver.api._

import collection.immutable.Iterable
import slick.lifted.ColumnOrdered
import slick.lifted.Ordered
import slick.ast.Ordering

import scala.util.Try

trait LightFilter[E, TE, T] extends FilterAPI[E] with FilterJoins[E, TE, T] {

  //################ Public API #####################

  /**
   * filter and sort all entities with given data
   */
  final override def filter(data: FilterDefinition)(implicit session: Session): Seq[E] =
    collector.collect(data, createFilter(data))


  override def filterWithTotalEntitiesNumber(data: FilterDefinition)(implicit session: Session): FilterResult[E] =
    collector.collectAndCount(data, createFilter(data))


  //################ Abstrat methods ##################

  def fieldFor(name: String): Option[FilterField]

  def baseQuery: FilterQuery

  def defaultOrder(q: T): Rep[_]

  def columnFor(q: T, name: String): Option[Rep[_]]

  def collector: Collector[E, TE, T]

  //################ Extension methods ##################

  protected def initialConstrains: Rep[Option[Boolean]] = LiteralColumn(Some(true)) //TODO include in dsl

  protected def noSuchColumn(name: String): Rep[_] =
    throw new IllegalArgumentException(s"Filter does not contain clumn $name") // TODO use specific exception

  protected def noSuchField(name: String): FilterField =
    throw new IllegalArgumentException(s"Filter does not contain field $name")

  protected def missingJoin(name: String): FilterQuery =
    throw new IllegalArgumentException(s"Filter does not join named: $name")

  //################ Internals ################

  private[filters] def filterOnQuery(data: FilterConstrains): FilterQuery =
    performJoins(baseQuery, data).filter(filters(data.fieldConstrains))

  private[filters]type FilterQuery = Query[T, TE, Seq]

  private def getField(name: String) = fieldFor(name).getOrElse(noSuchField(name))

  private def getColumn(q: T, name: String) = columnFor(q, name).getOrElse(noSuchColumn(name))

  protected def columnsFilters(liftedEntity: T, data: Map[String, Any]): Iterable[Rep[Option[Boolean]]] =
    data.map {
      case (name, value) =>
        getField(name).doFilter(getColumn(liftedEntity, name))(value)
    }

  /**
   * applies filter data into query where clauses
   */
  protected def filters(data: Map[String, Any])(liftedEntity: T): Rep[Option[Boolean]] =
    columnsFilters(liftedEntity, data).foldLeft(initialConstrains)(_ && _)

  def performJoins(t: FilterQuery, filterDefinition: FilterConstrains): FilterQuery =
    filterDefinition.nestedConstrains.foldLeft(t) {
      case (q, (name, data)) =>
        joins.get(name).map(_.apply(data)(q))
          .getOrElse(missingJoin(name))
    }

  private def ordering(data: FilterDefinition)(liftedEntity: T): Ordered = {
    def ordered(c: Rep[_]) = ColumnOrdered(c, Ordering())

    val fromFilter = data.orderBy.flatMap {
      case Order(name, asc) =>
        val column = ordered(getColumn(liftedEntity, name))

        (if (asc) column.asc else column.desc).columns
    }

    new Ordered(fromFilter ++ ordered(defaultOrder(liftedEntity)).asc.columns)
  }

  private def createFilter(data: FilterDefinition): FilterQuery =
    filterOnQuery(data.constrains).sortBy(ordering(data))
}
