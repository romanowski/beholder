package org.virtuslab.beholder.context

import org.virtuslab.unicorn.LongUnicornPlay._
import slick.driver.JdbcDriver
import slick.lifted.Rep

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, ExecutionContext}


class UnicornDatabaseContext(implicit session: driver.api.Session) extends DatabaseContext{
  override val jdbcDriver: JdbcDriver = driver

  override def run[E](query: Rep[E]): Future[E] = Future(query.run)

  override implicit val executionContext: ExecutionContext = ExecutionContext.Implicits.global

  def results[R](from: Databased[R]): R =
    Await.result(from.futureFunc(this), Duration.Inf) //NoOp - all is already computed
}
