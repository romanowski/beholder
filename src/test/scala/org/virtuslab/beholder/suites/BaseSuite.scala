package org.virtuslab.beholder.suites

import org.virtuslab.beholder.filters.{ FilterConstrains, FilterAPI, FilterDefinition }
import org.virtuslab.beholder.model._
import org.virtuslab.beholder.view.{UserMachineViewRow, UserMachinesView}
import org.virtuslab.beholder.{BaseTest, AppTest}
import org.virtuslab.unicorn.LongUnicornPlay.driver.api._

trait BaseSuite extends UserMachinesView with BaseTest {
  self: AppTest =>
  def createUserUserMachineFilter(data: BaseFilterData): FilterAPI[UserMachineViewRow]

  protected def baseFilterTest[A](testImplementation: BaseFilterData => A) = rollbackWithModel {
    implicit session: Session =>
      testImplementation(new BaseFilterData())
  }

  protected class BaseFilterData(implicit val session: Session) extends PopulatedDatabase {

    val view = createUsersMachineView

    lazy val userMachineFilter = createUserUserMachineFilter(this)

    lazy val baseFilter = FilterDefinition.empty

    def updatedDefinition(field: String, value: Any, definition: FilterDefinition = baseFilter) =
      definition.copy(
        constrains = definition.constrains.copy(
          fieldConstrains = definition.constrains.fieldConstrains + (field -> value)
        )
      )

    def addJoin(name: String, constrains: FilterConstrains, definition: FilterDefinition = baseFilter) =
      definition.copy(
        constrains = definition.constrains.copy(
          nestedConstrains = definition.constrains.nestedConstrains + (name -> constrains)
        )
      )

    lazy val allUserMachineRows: Seq[UserMachineViewRow] = view.list

    lazy val allProjects: Seq[Project] = TableQuery[Projects].list
  }

  def filterUserMachines(data: BaseFilterData, currentFilter: FilterDefinition): Seq[UserMachineViewRow] =
    doFilter(data.userMachineFilter, data, currentFilter)

  def doFilter[E](filter: FilterAPI[E], data: BaseFilterData, currentFilter: FilterDefinition): Seq[E] ={
    import data._
    val res = filter.filterWithTotalEntitiesNumber(currentFilter)
    res.content
  }
}
