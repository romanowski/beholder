package org.virtuslab.beholder.collectors

import org.virtuslab.beholder.filters._
import org.virtuslab.unicorn.LongUnicornPlay.driver.api._
import org.virtuslab.unicorn.LongUnicornPlay._
/*


case class Aggregated[E, A](from: E, data: Seq[A])



class OneToManyAggregator[E, A, T, TA](fromTable: Query[TA, A, Seq],
                                       joinConstrains: (T, TA) => Rep[Boolean])(
                                       implicit val teShape: Shape[FlatShapeLevel, T, E, T],
                                       implicit val taShape: Shape[FlatShapeLevel, TA, A, TA]
                                      ) extends Collector[Aggregated[E, A], E, T] {


  private def aggregate(fromDb: Seq[(E, Option[A])]): Seq[Aggregated[E, A]] = {
    fromDb match{
      case Nil => Nil
      case (e, a) +: tail =>
        tail.foldLeft(Seq(Aggregated(e, a.toSeq))){
          case (current +: rest, (e, Some(a))) if current.from == e =>
            Aggregated(e, current.data :+ a) +: rest
          case (current, (e, a)) =>
            Aggregated(e, a.toSeq) +: current
        }.reverse
    }
  }

  override def collect(data: FilterDefinition, query: Query[T, E, Seq])(implicit session: Session): Seq[Aggregated[E, A]] = {

    val afterTake = data.take.fold(query)(query.take)
    val afterSkip = data.skip.fold(afterTake)(afterTake.drop)

    val joinedQuery = afterSkip joinLeft fromTable on joinConstrains

    aggregate(joinedQuery.list)
  }

  override def collectAndCount(data: FilterDefinition, query: Query[T, E, Seq])(implicit session: Session): FilterResult[Aggregated[E, A]] = {
    val result = collect(data, query)
    val count = query.length.run

    FilterResult(result, count)
  }
}*/
