package org.virtuslab.beholder.context

import slick.dbio.Effect.All
import slick.dbio.{Effect, NoStream, DBIOAction}
import slick.driver.JdbcDriver
import slick.lifted.Rep

import scala.language.higherKinds

sealed trait DatabaseContext {
  val jdbcDriver: JdbcDriver

  def run[R](query: Rep[R]): Databased[R]
}

trait DatabaseContextImpl[RM[+_]] extends DatabaseContext {

  protected def resultFlatMap[A, B](on: RM[A])(func: A => RM[B]): RM[B]

  protected def resultMap[A, B](on: RM[A])(func: A => B): RM[B]

  def runQuery[E](q: Rep[E]): RM[E]

  def apply[E](e: E): RM[E]


  override def run[R](query: Rep[R]): Databased[R] = ContextedDatabased(runQuery(query))

  private case class ContextedDatabased[+E](result: RM[E]) extends Databased[E]{
    override def flatMap[A](mapper: (E) => Databased[A]): Databased[A] =
      ContextedDatabased(resultFlatMap(result)(mapper.andThen(runDatabased)))

    override def map[A](mapper: (E) => A): Databased[A] = ContextedDatabased(resultMap(result)(mapper))
  }

  def runDatabased[E](from: Databased[E]): RM[E] = from match{
    case NoOpDatabased(func) => apply(func(this))
    case NestedDatabased(func) => runDatabased(func(this))
    case cd: ContextedDatabased[E] => cd.result
  }
}


sealed trait Databased[+E] {
  def flatMap[A](mapper: E => Databased[A]): Databased[A]

  def map[A](mapper: E => A): Databased[A]
}


case class NoOpDatabased[+E](noOp: DatabaseContext => E) extends Databased[E]{
  override def flatMap[A](mapper: (E) => Databased[A]): Databased[A] =
    NestedDatabased(c => mapper(noOp(c)))

  override def map[A](mapper: (E) => A): Databased[A] =
    NoOpDatabased(noOp.andThen(mapper))
}

case class NestedDatabased[+E](nested: DatabaseContext => Databased[E]) extends Databased[E]{
  override def flatMap[A](mapper: (E) => Databased[A]): Databased[A] =
    NestedDatabased(c => nested(c).flatMap(mapper))

  override def map[A](mapper: (E) => A): Databased[A] =
    NestedDatabased(c => nested(c).map(mapper))
}


object Databased {

  def runQuery[E](query: Rep[E]): Databased[E] = NestedDatabased(c => c.run(query))

  def inContext[Q](func: JdbcDriver => Q): Databased[Q] = NoOpDatabased(c => func(c.jdbcDriver))

  def inTransaction[R](code: => Databased[R]): Databased[R] = ???
}
