package org.virtuslab.beholder.filters

import org.virtuslab.unicorn.LongUnicornPlay._
import org.virtuslab.unicorn.LongUnicornPlay.driver.api._

case class Aggregated[E, A](from: E, data: Seq[A])

class AggregatedFilter[E, A, TE, TA](
                                      fromFilter: LightFilter[E, E, TE],
                                      withFilter: LightFilter[A, A, TA],
                                      valueForName: (E, String) => Any,
                                      join: (TE, TA) => Rep[Boolean])(
        implicit val teShape: Shape[FlatShapeLevel, TE, E, TE],
      implicit val taShape: Shape[FlatShapeLevel, TA, A, TA]
) extends LightFilter[Aggregated[E, A], E, TE] {
  //TODO name mapping???

  override def fieldFor(name: String): Option[FilterField] =
    fromFilter.fieldFor(name)

  override def baseQuery: FilterQuery = fromFilter.baseQuery

  override def defaultOrder(q: TE): Rep[_] = fromFilter.defaultOrder(q)

  override def columnFor(q: TE, name: String): Option[Rep[_]] =
    fromFilter.columnFor(q, name)

  override protected def generateResults(fromDb: Seq[E]): Seq[Aggregated[E, A]] = ???

  private def mapRes(fromDb: Seq[(E, Option[A])]): Seq[Aggregated[E, A]] =
    fromDb.groupBy(_._1).map {
      case (e, toAggregate) =>
        Aggregated(e, toAggregate.flatMap(_._2)(collection.breakOut))
    }(collection.breakOut)

  override protected def takeAndSkip(data: FilterDefinition,
                                     filterQuery: FilterQuery)
                                    (implicit session: Session): Seq[Aggregated[E, A]] = {
   val original = super.takeAndSkip(data, filterQuery)

    def orderConstrains: ((TE) => Rep[Boolean]) = ???

    val joinedQuery = filterQuery.filter(orderConstrains) joinLeft withFilter.baseQuery on(join)

    mapRes(joinedQuery.list)
  }
}
