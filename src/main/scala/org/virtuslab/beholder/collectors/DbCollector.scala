package org.virtuslab.beholder.collectors

import org.virtuslab.beholder.filters.{FilterResult, FilterDefinition}
import org.virtuslab.unicorn.LongUnicornPlay.driver.api._
import org.virtuslab.unicorn.LongUnicornPlay._

class DbCollector[E, ET, T](mappingFunction: ET => E) extends Collector[E, ET, T]{

  private def takeAndSkip(data: FilterDefinition, filter: Query[T, ET, Seq]) = {
    val afterTake = data.take.fold(filter)(filter.take)
    data.skip.fold(afterTake)(afterTake.drop)
  }

  override def collect(data: FilterDefinition, query: Query[T, ET, Seq])(implicit session: Session): Seq[E] =
    takeAndSkip(data, query).list.map(mappingFunction)

  override def collectAndCount(data: FilterDefinition, query: Query[T, ET, Seq])(implicit session: Session): FilterResult[E] = {
    val results = collect(data, query)
    val count = query.length.run

    FilterResult(results, count)
  }
}
