package org.virtuslab.beholder.filters

import org.virtuslab.beholder.views.BaseView
import org.virtuslab.unicorn.LongUnicornPlay.driver.simple._

import scala.collection.immutable.Iterable
import scala.slick.ast.TypedCollectionTypeConstructor
import scala.slick.lifted.Ordered
import scala.util.Try

trait MappedColumnsFilter[E, T <: Table[E]] extends LightFilter[E, T] {

  protected val columns: Map[String, T => Column[_]]
  protected val order: T => Column[_]

  override protected def defaultOrder(q: T): Column[_] = order(q)

  override protected def columnFor(q: T, name: String): Option[Column[_]] = columns.get(name).map(_.apply(q))
}

trait ViewBasedFilter[E, T <: BaseView[E]] extends LightFilter[E, T] {
  override protected def columnFor(q: T, name: String): Option[Column[_]] = Try(q.columnByName(name)).toOption

  override protected def defaultOrder(q: T): Column[_] = q.id
}
trait MappedFieldsFilter {
  protected val fields: Map[String, FilterField]

  protected def fieldFor(name: String): Option[FilterField] = fields.get(name)
}

trait LightFilter[E, T <: Table[E]] extends FilterAPI[E] with FilterJoins[E, T] {

  //################ Public API #####################

  /**
   * filter and sort all entities with given data
   */
  final override def filter(data: FilterDefinition)(implicit session: Session): Seq[E] =
    takeAndSkip(data, createFilter(data))

  override def filterWithTotalEntitiesNumber(data: FilterDefinition)(implicit session: Session): FilterResult[E] = {
    val filter = createFilter(data)
    FilterResult(takeAndSkip(data, filter), filter.length.run)
  }

  //################ Abstrat methods ##################

  protected def fieldFor(name: String): Option[FilterField]

  protected def table: FilterQuery

  protected def defaultOrder(q: T): Column[_]

  protected def columnFor(q: T, name: String): Option[Column[_]]

  //################ Extendable methods ##################

  protected def initialConstrains: Column[Option[Boolean]] = LiteralColumn(Some(true))

  protected def noSuchColumn(name: String): Column[_] =
    throw new IllegalArgumentException(s"Filter does not contain clumn $name")

  protected def noSuchField(name: String): FilterField =
    throw new IllegalArgumentException(s"Filter does not contain field $name")

  protected def missingJoin(name: String): FilterQuery =
    throw new IllegalArgumentException(s"Filter does not join named: $name")

  //################ Internals ################

  private[filters] def filterOnQuery(data: FilterConstrains): FilterQuery =
    performJoins(table, data).filter(filters(data.fieldConstrains))

  private[filters]type FilterQuery = Query[T, T#TableElementType, Seq]

  private def getField(name: String) = fieldFor(name).getOrElse(noSuchField(name))

  private def getColumn(q: T, name: String) = columnFor(q, name).getOrElse(noSuchColumn(name))

  protected def columnsFilters(table: T, data: Map[String, Any]): Iterable[Column[Option[Boolean]]] =
    data.map {
      case (name, value) =>
        getField(name).doFilter(getColumn(table, name))(value)
    }

  /**
   * applies filter data into query where clauses
   */
  protected def filters(data: Map[String, Any])(table: T): Column[Option[Boolean]] =
    columnsFilters(table, data).foldLeft(initialConstrains)(_ && _)

  def performJoins(t: FilterQuery, filterDefinition: FilterConstrains): FilterQuery =
    filterDefinition.nestedConstrains.foldLeft(t) {
      case (q, (name, data)) =>
        joins.get(name).map(_.apply(data)(q))
          .getOrElse(missingJoin(name))
    }

  private def ordering(data: FilterDefinition)(table: T): Ordered = {
    val fromFilter = data.orderBy.flatMap {
      case Order(name, asc) =>
        val column = getColumn(table, name)
        (if (asc) column.asc else column.desc).columns
    }

    new Ordered(fromFilter ++ defaultOrder(table).asc.columns)
  }

  private def createFilter(data: FilterDefinition): FilterQuery =
    filterOnQuery(data.constrains).sortBy(ordering(data))

  private def takeAndSkip(data: FilterDefinition, filter: FilterQuery)(implicit session: Session): Seq[E] = {
    val afterTake = data.take.fold(filter)(filter.take)
    val afterSkip = data.skip.fold(afterTake)(afterTake.drop)

    afterSkip.to(TypedCollectionTypeConstructor.forArray).list
  }
}
