package org.virtuslab.beholder.filters

import org.virtuslab.beholder.consumers.StandardConsumer
import org.virtuslab.beholder.context.{Databased, Contexted}
import slick.lifted.{Query, Rep}

/**
  * Author: Krzysztof Romanowski
  */
trait BeholderFilter[E, T] extends (FilterDefinition => Databased[Query[T, E, Seq]]) {

  protected def filterFields: Map[String, FilterField]

  protected def filterColumns(from: T): Map[String, Rep[_]]

  protected def defaultOrder(from: T): Rep[_]
}

object BeholderFilter{
  implicit class consumedBeholderFilter[E, Filter <: BeholderFilter[E, _]](filter: Filter){
    def consumed = new StandardConsumer[E, Filter](filter)
  }
}
