package org.virtuslab.beholder.context

import slick.driver.JdbcDriver
import slick.lifted.Rep

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, ExecutionContext}


trait DatabaseContext {
  val jdbcDriver: JdbcDriver

  implicit val executionContext: ExecutionContext

  def run[E](query: Rep[E]): Future[E]
}

case class Databased[+E] private[context] (futureFunc: DatabaseContext => Future[E]){
  def map[A](mapper: E => A): Databased[A] = Databased(
    c => futureFunc(c).map(mapper)(c.executionContext)
  )

  def flatMap[A](mapper: E => Databased[A]): Databased[A] =
  Databased(c =>
      futureFunc(c).flatMap(e => mapper(e).futureFunc(c))(c.executionContext)
  )
}



object Databased {

  def runQuery[E](query: Rep[E]): Databased[E] = Databased(c => c.run(query))

  def inContext[Q](func: JdbcDriver => Q): Databased[Q] = Databased(c => Future(func(c.jdbcDriver))(c.executionContext))
}