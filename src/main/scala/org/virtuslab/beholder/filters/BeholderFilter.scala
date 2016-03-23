package org.virtuslab.beholder.filters

import slick.lifted.{Query, Rep}

/**
  * Author: Krzysztof Romanowski
  */
trait BeholderFilter[E, T] extends (FilterDefinition => Query[T, E, Seq]) {

  def allMatching(definition: FilterDefinition): Query[T, E, Seq]

  protected def filterFields: Map[String, FilterField]

  protected def filterColumns(from: T): Map[String, Rep[_]]

  protected def defaultOrder(from: T): Rep[_]
}


