package org.virtuslab.beholder.json

import java.sql.Date

import org.virtuslab.beholder.filters._
import org.virtuslab.beholder.model._
import org.virtuslab.beholder.suites._
import org.virtuslab.unicorn.LongUnicornPlay.driver.simple._
import org.virtuslab.beholder.filters.json.JsonDSL
import org.virtuslab.beholder._

class JsonDSLJoinFiltersTests extends AppTest with JoinSuite {
  import JsonDSL._

  override def createTeamFilter(data: BaseFilterData) =
    fromTable(TableQuery[Teams])(_.teamName) and
      "teamName" from (_.teamName) and
      "system" from (_.system) //TODO in Range

  override def createUserMachineFilter(data: BaseFilterData) =
    fromView(data.view) and
      "email" as in[String] and
      "system" as in[String] and
      "cores" as in[Int] and
      "created" as inRange[Date] and
      "capacity" as in[BigDecimal]
}

class JsonFiltersTests extends AppTest with FiltersTestSuite {
  def createFilter(data: BaseFilterData): FilterAPI[UserMachineViewRow] = {
    import JsonDSL._

    fromView(data.view) and
      "email" as in[String] and
      "system" as in[String] and
      "cores" as in[Int] and
      "created" as inRange[Date] and
      "capacity" as in[BigDecimal]

  }
}

class JsonFiltersRangeTests extends AppTest with RangeFiltersSuite {
  def createFilter(data: BaseFilterData): FilterAPI[UserMachineViewRow] = {
    import JsonDSL._

    fromView(data.view) and
      "email" as inText and
      "system" as inText and
      "cores" as inRange[Int] and
      "created" as inRange[Date] and
      "capacity" as inRange[BigDecimal]
  }
}