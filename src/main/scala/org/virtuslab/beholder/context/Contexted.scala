package org.virtuslab.beholder.context

import slick.driver.JdbcDriver
import slick.lifted.Rep

import scala.concurrent.{ExecutionContext, Future}



trait Contexted[+A] {
  val input: A

  val jdbcDriver: JdbcDriver

  implicit val executionContext: ExecutionContext

  def run[E](query: Rep[E]): Future[E]
}

