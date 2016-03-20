package org.virtuslab.beholder

import java.sql.Date

import org.virtuslab.beholder.filters._
import org.virtuslab.beholder.model._
import org.virtuslab.beholder.suites._
import org.virtuslab.beholder.view.UserMachineViewRow
import org.virtuslab.unicorn.LongUnicornPlay.driver.api._

class LightDSLJoinFiltersTests extends AppTest with JoinSuite {
  import LightDSLFilter._

  //TODO test table based filter
  //TODO test different DSLs

  override def createTeamFilter(data: BaseFilterData) =
    fromTable(TableQuery[Teams])(_.teamName) and
      "teamName" from (_.teamName) and
      "system" from (_.system)

  override def createUserMachineFilter(data: BaseFilterData) =
    fromView(data.view) and
      "email" as in[String] and
      "system" as in[String] and
      "cores" as in[Int] and
      "created" as in[Date] and
      "capacity" as in[BigDecimal]
}

class LightDSLFiltersTests extends AppTest with FiltersTestSuite {
  def createUserUserMachineFilter(data: BaseFilterData): FilterAPI[UserMachineViewRow] = {
    import LightDSLFilter._

    fromView(data.view) and
      "email" as in[String] and
      "system" as in[String] and
      "cores" as in[Int] and
      "created" as in[Date] and
      "capacity" as in[BigDecimal]
  }
}

class LightDSL2FiltersTests extends AppTest with FiltersTestSuite {
  def createUserUserMachineFilter(data: BaseFilterData): FilterAPI[UserMachineViewRow] = {
    import LightDSLFilter._

    fromView(data.view) and
      "email" as in[String] and
      "system" as in[String] and
      "cores" as in[Int] and
      "created" as in[Date] and
      "capacity" as in[BigDecimal]

  }
}