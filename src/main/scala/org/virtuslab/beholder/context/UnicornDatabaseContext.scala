package org.virtuslab.beholder.context

import org.virtuslab.unicorn.LongUnicornPlay._
import slick.driver.JdbcDriver
import slick.lifted.Rep

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, ExecutionContext}


case class NoOp[+E](v: E)

class UnicornDatabaseContext(implicit session: driver.api.Session) extends DatabaseContextImpl[NoOp]{

  override protected def resultFlatMap[A, B](on: NoOp[A])(func: (A) => NoOp[B]): NoOp[B] = func(on.v)

  override def apply[E](e: E): NoOp[E] = NoOp.apply(e)

  override protected def resultMap[A, B](on: NoOp[A])(func: (A) => B): NoOp[B] = NoOp(func(on.v))

  override def runQuery[E](q: Rep[E]): NoOp[E] = NoOp(q.run)

  override val jdbcDriver: JdbcDriver = driver
}