package org.virtuslab.beholder.consumers

import org.virtuslab.beholder.filters.{BeholderFilter, FilterDefinition, FilterResult}
import org.virtuslab.unicorn.LongUnicornPlay.driver.api._
import org.virtuslab.unicorn.LongUnicornPlay._

class FilterResultConsumer[E, T](override val filter: BeholderFilter[E, T])(implicit session: Session)
  extends FilterAwareConsumer[E, T, FilterResult[E]]{
  override def apply(definition: FilterDefinition): FilterResult[E] = {
    val results = filter(definition).list
    val count = filter.allMatching(definition).length.run

    FilterResult(results, count)
  }
}
