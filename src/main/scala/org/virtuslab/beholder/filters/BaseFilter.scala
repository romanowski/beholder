package org.virtuslab.beholder.filters

import scala.language.postfixOps
import scala.slick.ast.TypedType
import scala.slick.lifted.{LiteralColumn, TableQuery}

import org.virtuslab.beholder.views.BaseView
import org.virtuslab.beholder.views.FilterableViews.BaseView2
import play.api.data.{Form, Mapping}
import play.api.data.Forms._
import play.api.db.slick.Config.driver.simple._

/**
 * base class that is mapped to form
 * it contain all common and specific (data field of genric type D) filter data
 * @param take
 * @param skip
 * @param orderBy
 * @param asc
 * @param data
 * @tparam Data type of filter data
 */
case class BaseFilterEntity[Data](take: Option[Int],
                                  skip: Option[Int],
                                  orderBy: Option[String],
                                  asc: Boolean,
                                  data: Data)

/**
 * base filter class - it contains public operations for all filters instances
 * @param table table to filter on
 * @tparam Id table id
 * @tparam Entity table entity
 * @tparam Table table class (usually View.type)
 * @tparam FilteredData filter data type (usually tuple with data)
 */
abstract class BaseFilter[Id, Entity, Table <: BaseView[Id, Entity], FilteredData](val table: TableQuery[Table]) {

  /**
   * from mapping for this filter
   * @return
   */
  protected def filterMapping: Mapping[BaseFilterEntity[FilteredData]]

  /**
   * Empty data for filter representing empty filter (all fields in tuple (type M) are filled with Empty)
   */
  protected def emptyFilterDataInner: FilteredData

  /**
   * applies filter data into query where clauses
   * @param data
   * @param table
   * @return
   */
  protected def filters(data: FilteredData)(table: Table): Column[Option[Boolean]]

  /**
   * @return data representing empty filter - query for all entities in table
   */
  final def emptyFilterData = BaseFilterEntity(None, None, None, asc = true, emptyFilterDataInner)

  /**
   * form for this filter
   * @return
   */
  final def filterForm = Form(filterMapping)


  /**
   * filter and sort all entities with given data
   * @param data
   * @param session
   * @return
   */
  final def filter(data: BaseFilterEntity[FilteredData])(implicit session: Session): Seq[Entity] = {
    val base = table.filter(filters(data.data)).sortBy {
      inQueryTable =>
        val globalColumns =
          order(data)(inQueryTable).map(c =>
            if (data.asc) c.asc else c.desc
          ).toSeq.flatMap(_.columns)
        new scala.slick.lifted.Ordered(globalColumns ++ inQueryTable.id.asc.columns)
    }

    val afterTake = data.take.fold(base)(base.take)
    val afterSkip = data.skip.fold(afterTake)(afterTake.drop)

    afterSkip.list
  }

  //ordering
  private def order(data: BaseFilterEntity[FilteredData])(table: Table): Option[Column[_]] =
    data.orderBy.flatMap(table.columnByName(table))
}

/**
 * Generates inners of FiltersGeneratedCode class
 */
object FiltersGenerator extends App {
  final def generateCode = {
    (3 to 18).map {
      implicit nr =>
        import org.virtuslab.beholder.utils.CodeGenerationUtils._

        val fieldFilters = fill(nr => s"c${nr}Mapping: FilterField[A$nr, B$nr]", ",\n")
        val options = fill(nr => s"Option[B$nr]")
        val columnsNames = fill("c" +)
        val filterMappings = fill(nr => s"realTable.columnNames(${nr - 1}) -> optional(c${nr}Mapping.mapping)", ",\n")
        val queryFilters = fill(nr => s"c$nr.map(c${nr}Mapping.filterOnColumn(realTable.c$nr))", ",\n")
        val nones = fill(nr => "None")
        s"""
          |def create[$aTypesWithTypedType, $bTypes, T <: BaseView$nr[Entity, $aTypes]](table: TableQuery[T],
          |                                                                   $fieldFilters
          | ): BaseFilter[A1, Entity, T, ($options)] = {
          |
          |    def obtainRealTable = table.shaped.value
          |
          |    new BaseFilter[A1, Entity, T, ($options)](table) {
          |
          |      private val realTable = obtainRealTable
          |
          |      def filterMapping: Mapping[BaseFilterEntity[($options)]] = baseFilterEntityMapping(tuple(
          |        $filterMappings
          |      ))
          |
          |      override protected def emptyFilterDataInner: ($options) = ($nones)
          |
          |      protected def filters(data: ($options))(table: T): Column[Option[Boolean]] = data match {
          |        case ($columnsNames) =>
          |          Seq(
          |            $queryFilters
          |          ).flatten.foldLeft(LiteralColumn(Some(true)): Column[Option[Boolean]]) {
          |            _ && _
          |          }
          |      }
          |    }
          |  }""".stripMargin
    }
  }

  println(generateCode.mkString("\n"))
}

class FiltersGenerator[Entity] extends FiltersGeneratedCode[Entity] {

  /**
   * create filter for 2 fields view
   * All other create method are base on this one
   * @param table view for filter on
   * @param c1Mapping mapping for first field
   * @param c2Mapping mapping for sec field
   * @tparam A1 type of first field
   * @tparam A2 type of sec field
   * @tparam T type of view (View.type)
   * @tparam B1 filter data types for first field
   * @tparam B2 filter data types for sec field
   * @return
   */
  final def create[A1: TypedType, A2: TypedType, T <: BaseView2[Entity, A1, A2], B1, B2](table: TableQuery[T],
                                                                                         c1Mapping: FilterField[A1, B1],
                                                                                         c2Mapping: FilterField[A2, B2]): BaseFilter[A1, Entity, T, (Option[B1], Option[B2])] = {

    def obtainRealTable = table.shaped.value

    new BaseFilter[A1, Entity, T, (Option[B1], Option[B2])](table) {
      private val realTable = obtainRealTable

      def filterMapping: Mapping[BaseFilterEntity[(Option[B1], Option[B2])]] = baseFilterEntityMapping(tuple(
        realTable.columnNames(0) -> optional(c1Mapping.mapping),
        realTable.columnNames(1) -> optional(c2Mapping.mapping)
      ))

      override protected def emptyFilterDataInner: (Option[B1], Option[B2]) = (None, None)

      protected def filters(data: (Option[B1], Option[B2]))(table: T): Column[Option[Boolean]] = data match {
        case (c1, c2) =>
          val realTable = obtainRealTable
          Seq(
            c1.map(c1Mapping.filterOnColumn(realTable.c1)),
            c2.map(c2Mapping.filterOnColumn(realTable.c2))
          ).flatten.foldLeft(LiteralColumn(Some(true)): Column[Option[Boolean]]) {
            _ && _
          }
      }
    }
  }
}
