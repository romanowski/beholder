package org.virtuslab.beholder.context

import slick.driver.JdbcDriver
import slick.lifted.Rep

trait ResultMonad[A]{
  def flatMap[B](func: A => ResultMonad[B]): ResultMonad[B]

  def map[B](func: A => B): ResultMonad[B]
}

trait Contexted[+A] {
  val input: A

  val driver: JdbcDriver

  def run[E](query: Rep[E]): ResultMonad[E]
}
