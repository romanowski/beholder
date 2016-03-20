package org.virtuslab.beholder.suites

import java.sql.Date

import org.joda.time.DateTime
import org.virtuslab.beholder.AppTest
import org.virtuslab.beholder.filters._

trait FiltersTestSuite extends BaseSuite with RangeFiltersSuite {
  self: AppTest =>


  // TODO add enums
  // TODO add negative tests
  /*
    UserMachineViewRow(a@a.pl,Ubuntu,4,2014-12-05,Some(1.00))
    UserMachineViewRow(o@a.pl,Ubuntu,4,2014-12-05,Some(1.00))
    UserMachineViewRow(o@a.pl,Fedora,1,2014-12-05,Some(3.00))
   */

  it should "query all entities for empty filter" in baseFilterTest {
    data =>
      import data._
      val all = filterUserMachines(data, baseFilter)

      all should contain theSameElementsAs allUserMachineRows
  }

  it should "order by argument asc correctly" in baseFilterTest {
    data =>
      import data._

      val fromDbOrderedByCores = allUserMachineRows.sortBy(view => (view.cores, view.email))
      val orderByCore = filterUserMachines(data, baseFilter.withOrder("cores"))

      orderByCore should contain theSameElementsInOrderAs fromDbOrderedByCores
  }

  it should "order by argument desc correctly" in baseFilterTest {
    data =>
      import data._
      val orderByCoreDesc = filterUserMachines(data, baseFilter.withOrder("cores", asc = false))
      val fromDbOrderedByCoresDesc = allUserMachineRows.sortBy(view => (-view.cores, view.email))

      orderByCoreDesc should contain theSameElementsInOrderAs fromDbOrderedByCoresDesc
  }

  it should "take correctly" in baseFilterTest {
    data =>
      import data._
      val orderByCoreDesc = filterUserMachines(data, baseFilter.withOrder("cores", asc = false).copy(take = Some(2)))
      val fromDbOrderedByCoresDesc = allUserMachineRows.sortBy(view => (-view.cores, view.email))

      orderByCoreDesc should contain theSameElementsInOrderAs fromDbOrderedByCoresDesc.take(2)
  }

  it should "skip correctly" in baseFilterTest {
    data =>
      import data._
      val orderByCoreDesc = filterUserMachines(data, baseFilter.withOrder("cores", asc = false).copy(skip = Some(1)))
      val fromDbOrderedByCoresDesc = allUserMachineRows.sortBy(view => (-view.cores, view.email))

      orderByCoreDesc should contain theSameElementsInOrderAs fromDbOrderedByCoresDesc.drop(1)
  }

  it should "filter by int field" in baseFilterTest {
    data =>
      import data._
      val orderByCoreDesc = filterUserMachines(data, updatedDefinition("cores", 4))
      val fromDbOrderedByCoresDesc = allUserMachineRows.filter(_.cores == 4)

      orderByCoreDesc should contain theSameElementsInOrderAs fromDbOrderedByCoresDesc
  }

  //h2db does not have ilike operator
  ignore should "filter by string field" in baseFilterTest {
    data =>
      import data._
      val newSystem = "buntu"
      val orderByCoreDesc = filterUserMachines(data, updatedDefinition("system", newSystem))
      val fromDbOrderedByCoresDesc = allUserMachineRows.filter(_.system.contains(newSystem))

      orderByCoreDesc should contain theSameElementsInOrderAs fromDbOrderedByCoresDesc
  }

  it should "not crash for date option" in baseFilterTest {
    data =>
      import data._
      val toDate = new Date(DateTime.now().minusHours(24).getMillis)
      val dataRange = FilterRange(None, Some(toDate))

      val newVersion = updatedDefinition("created", dataRange)
      val fromdbWithCorrectDates = allUserMachineRows.filter(_.created.before(toDate))

      val withCorrectDates = filterUserMachines(data, newVersion)
      withCorrectDates should contain theSameElementsInOrderAs fromdbWithCorrectDates
  }

  it should "skip correctly and return correct total amount of entities" in baseFilterTest {
    data =>
      import data._

      val filterData = userMachineFilter.filterWithTotalEntitiesNumber(
        baseFilter.withOrder("cores", asc = false).copy(skip = Some(1))
      )
      val fromDbOrderedByCoresDesc = allUserMachineRows.sortBy(view => (-view.cores, view.email)).drop(1)

      filterData.content should contain theSameElementsInOrderAs fromDbOrderedByCoresDesc

      filterData.total shouldEqual allUserMachineRows.size

  }
}