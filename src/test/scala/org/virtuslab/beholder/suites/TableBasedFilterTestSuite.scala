package org.virtuslab.beholder.suites

import org.virtuslab.beholder.AppTest
import org.virtuslab.beholder.filters.FilterAPI
import org.virtuslab.beholder.model.Project

trait TableBasedFilterTestSuite extends BaseSuite {
  self: AppTest =>

  def createProjectFilter(data: BaseFilterData): FilterAPI[Project]

  it should "query all entities for empty filter" in baseFilterTest {
    data =>
      import data._
      val all = doFilter(createProjectFilter(data), data, baseFilter)

      all should contain theSameElementsAs allProjects
  }

  it should "filter by optional id field" in baseFilterTest {
    data =>
      val teamId = data.teams.head.id.get

      import data._
      val filteredTeams = filterUserMachines(data, updatedDefinition("team", teamId))
      val withTeam = allProjects.filter(_.team == Some(teamId))

      filteredTeams should contain theSameElementsInOrderAs withTeam
  }


}
