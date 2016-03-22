package org.virtuslab.beholder.suites

import org.virtuslab.beholder.filters.{FilterResult, FilterConstrains, FilterAPI, FilterDefinition}
import org.virtuslab.beholder.model._
import org.virtuslab.beholder.view.{ UserMachineViewRow, UserMachinesView }
import org.virtuslab.beholder.{ BaseTest, AppTest }
import org.virtuslab.unicorn.LongUnicornPlay.driver.api._

trait BaseSuite[E] extends UserMachinesView with BaseTest {

  def createUserMachinesFilter(data: BaseFilterData): FilterAPI[E]

  protected def baseFilterTest[A](testImplementation: BaseFilterData => A) = rollbackWithModel {
    implicit session: Session =>
      testImplementation(new BaseFilterData())
  }

  protected def compare(result: FilterResult[E], expected: Seq[UserMachineViewRow], data: BaseFilterData): Unit =
    result.content should contain theSameElementsAs expected


  protected class BaseFilterData(implicit val session: Session) extends PopulatedDatabase {

    case class filtering(fromFilter: FilterDefinition) {
      def shouldResultIn(expected: Seq[UserMachineViewRow]): Unit = {
        val result = doFullFilter(BaseFilterData.this, fromFilter)

        val dropedAndSkiped = {
          val dropped = expected.drop(fromFilter.skip.getOrElse(0))
          fromFilter.take.map(dropped.take).getOrElse(dropped)
        }

        compare(result, dropedAndSkiped, BaseFilterData.this)

        //TODO - test for that!
         result.total shouldEqual expected.size
      }
    }

    val view = createUsersMachineView

    def updatedDefinition(field: String, value: Any, definition: FilterDefinition = FilterDefinition.empty) =
      definition.copy(
        constrains = definition.constrains.copy(
          fieldConstrains = definition.constrains.fieldConstrains + (field -> value)
        )
      )

    def addJoin(name: String, constrains: FilterConstrains, definition: FilterDefinition = FilterDefinition.empty) =
      definition.copy(
        constrains = definition.constrains.copy(
          nestedConstrains = definition.constrains.nestedConstrains + (name -> constrains)
        )
      )

    lazy val allUserMachineRows: Seq[UserMachineViewRow] = view.list

    lazy val allProjects: Seq[Project] = TableQuery[Projects].list
  }

  def doFullFilter(data: BaseFilterData, currentFilter: FilterDefinition): FilterResult[E] = {
    import data._
    createUserMachinesFilter(data).filterWithTotalEntitiesNumber(currentFilter)
  }

  def doFilter(data: BaseFilterData, currentFilter: FilterDefinition): Seq[E] = {
    doFullFilter(data, currentFilter).content
  }
}
