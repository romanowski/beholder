package org.virtuslab.beholder.filters

import org.virtuslab.beholder.context.Databased
import slick.driver.JdbcDriver


import slick.lifted.ColumnOrdered
import slick.lifted._
import slick.ast.Ordering


trait LightFilter[E, T] extends BeholderFilter[E, T] with FilterJoins[E, T] {

  //################ Public API #####################


  override def apply(definition: FilterDefinition): Databased[Query[T, E, Seq]] = Databased.inContext{
    driver =>
      filterOnQuery(definition.constrains, driver).sortBy(ordering(definition))
  }


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


  private[filters] def filterOnQuery(constrains: FilterConstrains, driver: JdbcDriver): FilterQuery = {
    val joined = performJoins(baseQuery, constrains, driver)

    if(constrains.fieldConstrains.isEmpty)
      joined
    else
      joined.filter(columnConstraints(constrains.fieldConstrains, driver))
  }



  protected def columnConstraints(data: Map[String, Any], driver: JdbcDriver)(liftedEntity: T): Rep[Option[Boolean]]= {
    val columns = filterColumns(liftedEntity)
    val fields = filterFields

    import driver.api._

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

  def performJoins(t: FilterQuery, constrains: FilterConstrains, driver: JdbcDriver): FilterQuery =
    constrains.nestedConstrains.foldLeft(t) {
      case (q, (name, data)) =>
        joins.get(name).map(_.apply(data, driver)(q))
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
