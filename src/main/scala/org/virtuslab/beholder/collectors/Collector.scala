package org.virtuslab.beholder.collectors

import org.virtuslab.beholder.filters.{FilterResult, FilterDefinition}
import org.virtuslab.unicorn.LongUnicornPlay.driver.api._

trait Collector[E, TE, T] {

  def collect(data: FilterDefinition, query: Query[T,  TE, Seq])(implicit session: Session): Seq[E]

  def collectAndCount(data: FilterDefinition, query: Query[T,  TE, Seq])(implicit session: Session): FilterResult[E]
}
