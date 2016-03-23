package org.virtuslab.beholder.filters

import org.virtuslab.unicorn.LongUnicornPlay.driver.api._


import slick.lifted.ColumnOrdered
import slick.lifted.{Ordered, Query}
import slick.ast.Ordering

import scala.util.Try

trait LightFilter[E, T] extends BeholderFilter[E, T] with FilterJoins[E, T] {

  //################ Public API #####################


  override def apply(definition: FilterDefinition): Query[T, E, Seq] = {
    val all = allMatching(definition)

    val afterSkip = definition.skip.fold(all)(all.drop)
    definition.take.fold(afterSkip)(afterSkip.take)
  }

  override def allMatching(definition: FilterDefinition): Query[T, E, Seq] =
    filterOnQuery(definition.constrains).sortBy(ordering(definition))


  //################ Abstrat methods ##################



  def baseQuery: FilterQuery

  //################ Extension methods ##################

  protected def noSuchColumn(name: String): Rep[_] =
    throw new IllegalArgumentException(s"Filter does not contain clumn $name") // TODO use specific exception

  protected def noSuchField(name: String): FilterField =
    throw new IllegalArgumentException(s"Filter does not contain field $name")

  protected def missingJoin(name: String): FilterQuery =
    throw new IllegalArgumentException(s"Filter does not join named: $name")

  //################ Internals ################

  type FilterQuery = Query[T, E, Seq]


  private[filters] def filterOnQuery(data: FilterConstrains): FilterQuery = {
    val joined = performJoins(baseQuery, data)

    if(data.fieldConstrains.isEmpty)
      joined
    else
      joined.filter(columnConstraints(data.fieldConstrains))
  }



  protected def columnConstraints(data: Map[String, Any])(liftedEntity: T): Rep[Option[Boolean]]= {
    val columns = filterColumns(liftedEntity)
    val fields = filterFields

    val fieldsReps = data.map {
      case (name, value) =>
        val field = fields.getOrElse(name, noSuchField(name))
        val column = columns.getOrElse(name, noSuchColumn(name))

        field.doFilter(column)(value)
    }

    fieldsReps.toSeq match{
      case Seq(rep) => rep
      case rep +: tail => tail.foldLeft(rep)(_ && _)
    }

  }

  def performJoins(t: FilterQuery, filterDefinition: FilterConstrains): FilterQuery =
    filterDefinition.nestedConstrains.foldLeft(t) {
      case (q, (name, data)) =>
        joins.get(name).map(_.apply(data)(q))
          .getOrElse(missingJoin(name))
    }

  private def ordering(data: FilterDefinition)(liftedEntity: T): Ordered = {
    def ordered(c: Rep[_]) = ColumnOrdered(c, Ordering())

    val columns = filterColumns(liftedEntity)

    val fromFilter = data.orderBy.flatMap {
      case Order(name, asc) =>
        val column = ordered(columns.getOrElse(name, noSuchColumn(name)))

        (if (asc) column.asc else column.desc).columns
    }

    new Ordered(fromFilter ++ ordered(defaultOrder(liftedEntity)).asc.columns)
  }
}
