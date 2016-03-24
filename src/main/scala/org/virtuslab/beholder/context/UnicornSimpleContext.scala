package org.virtuslab.beholder.context

import slick.driver.JdbcDriver
import slick.lifted.Rep

case class StupidMonad[A](value: A) extends ResultMonad[A]{
  override def flatMap[B](func: (A) => ResultMonad[B]): ResultMonad[B] = func(value)

  override def map[B](func: (A) => B): ResultMonad[B] = StupidMonad(func(value))
}

import org.virtuslab.unicorn.LongUnicornPlay._

case class UnicornSimpleContext[A](input: A)(implicit session: driver.api.Session) extends Contexted[A]{

  override def run[E](query: Rep[E]): ResultMonad[E] = {
    StupidMonad(query.run)
  }

  override val driver: JdbcDriver = driver

}
