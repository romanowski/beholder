package org.virtuslab.beholder.suites

import org.virtuslab.beholder.BaseTest
import org.virtuslab.beholder.filters.{FilterDefinition, LightFilter}
import org.virtuslab.beholder.view.UserMachineViewRow
import org.virtuslab.beholder.views.BaseView
import org.virtuslab.unicorn.LongUnicornPlay._

trait MappedCollectorTest[E] extends BaseTest {
  self: BaseSuite =>


  def createFilter(data: BaseFilterData): LightFilter[E, _]

  def compare(data: BaseFilterData, fromFilter: Seq[E], expected: Seq[UserMachineViewRow]): Unit

  override def testResults(data: BaseFilterData,
                           definition: FilterDefinition,
                           expected: Seq[UserMachineViewRow],
                           totalCount: Int): Unit = {
    import data._

    val filter = createFilter(data)
    val filtered = filter.apply(definition).list

    compare(data, filtered, expected)

    val counted = filter.allMatching(definition).length.run

    counted shouldEqual totalCount
  }

}


trait DefaultCollectorTest extends MappedCollectorTest[UserMachineViewRow] { //TODO better name
  self: BaseSuite =>
  override def compare(data: BaseFilterData,
                       fromFilter: Seq[UserMachineViewRow],
                       expected: Seq[UserMachineViewRow]): Unit = {
    fromFilter should contain theSameElementsAs expected
  }
}
