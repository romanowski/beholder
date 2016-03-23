package org.virtuslab.beholder.consumers

import org.virtuslab.beholder.filters.{BeholderFilter, FilterDefinition}
import slick.lifted.Query

trait Consumer[R] extends (FilterDefinition => R)

trait FilterAwareConsumer[E, T, R] extends Consumer[R]{
  def filter: BeholderFilter[E, T]
}


