package org.virtuslab.beholder.filters

import org.virtuslab.unicorn.LongUnicornPlay.driver.simple._

trait FilterJoins[E, T <: Table[E]] {
  self: LightFilter[E, T] =>

  type FilterJoin = FilterConstrains => FilterQuery => FilterQuery

  var _joins: Map[String, FilterJoin] = Map.empty

  protected def joins: Map[String, FilterJoin] = _joins

  //TODO - dsl
  def join[TE, TT <: Table[TE]](name: String, from: LightFilter[TE, TT])(on: (T, TT) => Column[Boolean]): LightFilter[E, T] = {
    val join: FilterJoin = data => query =>
      for {
        ft <- query
        tt <- from.filterOnQuery(data) if on(ft, tt)
      } yield ft

    _joins += name -> join
    this
  }
}
