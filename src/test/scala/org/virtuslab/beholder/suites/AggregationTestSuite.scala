package org.virtuslab.beholder.suites

import org.virtuslab.beholder.AppTest
import org.virtuslab.beholder.collectors.Aggregated
import org.virtuslab.beholder.filters._
import org.virtuslab.beholder.model.{MachineParameter, UserId}
import org.virtuslab.beholder.view.UserMachineViewRow

case class SystemPerUsers(system: String, users: Seq[UserId])

trait AggregationTestSuite extends AbstractFiltersTestSuite[Aggregated[UserMachineViewRow, MachineParameter]] {
  self: AppTest =>

    //TODO what with total entities number in aggregation case?

  private def aggregate(res: Seq[UserMachineViewRow]): Seq[SystemPerUsers] =
    res.groupBy(_.system).map {
      case (system, rows) =>
        SystemPerUsers(system, rows.map(_.userId))
    }.toSeq


  override protected def compare(result: FilterResult[Aggregated[UserMachineViewRow, MachineParameter]],
                                 expected: Seq[UserMachineViewRow],
                                 data: BaseFilterData): Unit = {
    val aggregated = expected.map{
      userMachine =>
        Aggregated(userMachine, data.machineParameters.filter(_.machine == userMachine.machineId))
    }

    result.content should contain theSameElementsAs aggregated

  }

}
