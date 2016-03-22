package org.virtuslab.beholder.collectors

import org.virtuslab.beholder.filters._
import org.virtuslab.unicorn.LongUnicornPlay.driver.api._
import org.virtuslab.unicorn.LongUnicornPlay._


case class Aggregated[E, A](from: E, data: Seq[A])



class InMemoryAggregator[E, A, T, TA](fromFilter: LightFilter[E, E, T],
                                      withFilter: LightFilter[A, A, TA],
                                      joinConstrains: (T, TA) => Rep[Boolean])(
                                       implicit val teShape: Shape[FlatShapeLevel, T, E, T],
                                       implicit val taShape: Shape[FlatShapeLevel, TA, A, TA]
                                     ) extends Collector[Aggregated[E, A], E, T] {


  private def aggregate(fromDb: Seq[(E, Option[A])]): Seq[Aggregated[E, A]] =
    fromDb.groupBy(_._1).map {
      case (e, toAggregate) =>
        Aggregated(e, toAggregate.flatMap(_._2)(collection.breakOut))
    }(collection.breakOut)

  override def collect(data: FilterDefinition, query: Query[T, E, Seq])(implicit session: Session): Seq[Aggregated[E, A]] = {
    val joinedQuery = query joinLeft withFilter.baseQuery on joinConstrains
    val all = aggregate(joinedQuery.list)

    val skipped = all.drop(data.skip.getOrElse(0))
    data.take.fold(skipped)(skipped.take)
  }

  override def collectAndCount(data: FilterDefinition, query: Query[T, E, Seq])(implicit session: Session): FilterResult[Aggregated[E, A]] = {
    val result = collect(data, query)
    val count = query.length.run

    FilterResult(result, count)
  }
}