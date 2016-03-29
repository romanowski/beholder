package org.virtuslab.beholder.context

import slick.driver.JdbcDriver
import slick.lifted.Rep

import org.virtuslab.unicorn.LongUnicornPlay._

import scala.concurrent.{ExecutionContext, Future}

case class UnicornSimpleContext[A](input: A)(implicit session: driver.api.Session) extends Contexted[A]{

  override implicit val executionContext: ExecutionContext = ExecutionContext.Implicits.global

  override def run[E](query: Rep[E]): Future[E] = Future(query.run)

  override val jdbcDriver: JdbcDriver = driver

}
