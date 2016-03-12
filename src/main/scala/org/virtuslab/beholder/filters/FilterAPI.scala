package org.virtuslab.beholder.filters
import org.virtuslab.unicorn.LongUnicornPlay.driver.api._

trait FilterAPI[Entity] {

  def filter(data: FilterDefinition)(implicit session: Session): Seq[Entity]

  def filterWithTotalEntitiesNumber(data: FilterDefinition)(implicit session: Session): FilterResult[Entity]
}