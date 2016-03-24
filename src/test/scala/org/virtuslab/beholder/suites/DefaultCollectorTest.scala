package org.virtuslab.beholder.suites


import org.virtuslab.beholder.BaseTest
import org.virtuslab.beholder.consumers.{QueryConsumers, Consumer}
import org.virtuslab.beholder.context.{StupidMonad, UnicornSimpleContext}
import org.virtuslab.beholder.filters.{FilterResult, FilterDefinition, LightFilter}
import org.virtuslab.beholder.view.UserMachineViewRow
import org.virtuslab.beholder.views.BaseView
import org.virtuslab.unicorn.LongUnicornPlay._
import slick.lifted.{Query, Rep}

trait MappedCollectorTest[E, R] extends BaseTest {
  self: BaseSuite =>


  def collector: Consumer[FilterDefinition, Query[_, E, Seq], R]

  def createFilter(data: BaseFilterData): LightFilter[E, _]

  def compare(data: BaseFilterData, collected: R, expected: Seq[UserMachineViewRow], totalCounnt: Int): Unit

  override def testResults(data: BaseFilterData,
                           definition: FilterDefinition,
                           expected: Seq[UserMachineViewRow],
                           totalCount: Int): Unit = {
    import data._

    val context = UnicornSimpleContext(definition)

    val filter = createFilter(data)

    val result = collector.consume(filter).apply(context).asInstanceOf[StupidMonad[R]].value

    compare(data, result, expected, totalCount)
  }

}


trait DefaultCollectorTest extends MappedCollectorTest[UserMachineViewRow, FilterResult[UserMachineViewRow]] { //TODO better name
  self: BaseSuite =>
  override def collector: Consumer[FilterDefinition, Query[_, UserMachineViewRow, Seq], FilterResult[UserMachineViewRow]] =
    QueryConsumers.simplePagination

  override def compare(data: BaseFilterData,
                       collected: FilterResult[UserMachineViewRow],
                       expected: Seq[UserMachineViewRow],
                       totalCounnt: Int): Unit = {
    collected.total shouldEqual totalCounnt
    collected.content should contain theSameElementsAs expected
  }
}
