package org.virtuslab.beholder.suites

import org.virtuslab.beholder.AppTest
import org.virtuslab.beholder.filters._
import org.virtuslab.beholder.model.UserId
import org.virtuslab.beholder.view.UserMachineViewRow

case class SystemPerUsers(system: String, users: Seq[UserId])

trait AggregationTestSuite extends AbstractFiltersTestSuite[SystemPerUsers] {
  self: AppTest =>


  //TODO what with total entities number in aggregation case?

  private def aggregate(res: Seq[UserMachineViewRow]): Seq[SystemPerUsers] =
    res.groupBy(_.system).map {
      case (system, rows) =>
        SystemPerUsers(system, rows.map(_.userId))
    }.toSeq


  override protected def compare(result: FilterResult[SystemPerUsers], expected: Seq[UserMachineViewRow]): Unit =
    result.content should contain theSameElementsAs aggregate(expected)

}
