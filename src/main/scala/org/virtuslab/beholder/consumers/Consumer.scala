package org.virtuslab.beholder.consumers

import org.virtuslab.beholder.context.{ResultMonad, Contexted}
import org.virtuslab.beholder.filters.{FilterResult, SimplePagination, BeholderFilter, FilterDefinition}
import play.api.libs.json.{JsValue, Writes}
import slick.lifted.{Rep, Query}


trait Consumer[-InputContext, -Q, R] {
  def consume[I <: InputContext](query: Contexted[I] => Q): Contexted[I] => ResultMonad[R]
}

object Consumer{

  def liftInput[IC] = new LiftInput[IC]

  class LiftInput[IC]{
    def transformQuery[Q, R](consumer: Consumer[IC, Q, R])(mapQuery: IC => Q => Q) = new Consumer[IC, Q, R]{
      override def consume[I <: IC](query: (Contexted[I]) => Q): (Contexted[I]) => ResultMonad[R] =
        consumer.consume(c => mapQuery(c.input)(query(c)))
    }
  }

  implicit class ConsumerOps[IC, Q, R](consumer: Consumer[IC, Q, R]){
    def generalMapResult[NR](mapper: ResultMonad[R] => ResultMonad[NR]) = new Consumer[IC, Q, NR] {
      override def consume[I <: IC](query: (Contexted[I]) => Q): (Contexted[I]) => ResultMonad[NR] =
        consumer.consume(query).andThen(mapper)
    }

    def mapResult[NR](mapper: R => NR): Consumer[IC, Q, NR] = generalMapResult((x: ResultMonad[R])=> x.map(mapper))

    def flatMapResult[NR](mapper: R => ResultMonad[NR]): Consumer[IC, Q, NR] = generalMapResult((x: ResultMonad[R])=> x.flatMap(mapper))
  }

  def merge[IC, Q, R1, R2, R](c1: Consumer[IC, Q, R1], c2: Consumer[IC, Q, R2])(merge: (R1, R2)=> R) = new Consumer[IC, Q, R] {
    override def consume[I <: IC](query: (Contexted[I]) => Q): (Contexted[I]) => ResultMonad[R] = {
      (i: Contexted[I]) =>
        val rm1 = c1.consume[I](query)(i)
        val rm2 = c2.consume[I](query)(i)

        rm1.flatMap(r1 => rm2.map(r2 => merge(r1, r2)))
    }
  }
}

object QueryConsumers{
  object length extends Consumer[Any,Query[_, _, Seq], Int]{
    override def consume[I](query: (Contexted[I]) => Query[_, _, Seq]): (Contexted[I]) => ResultMonad[Int] =
      (i: Contexted[I]) => i.run(query(i).length)
  }

  def list[E] = new Consumer[Any,Query[_, E, Seq], Seq[E]]{
    override def consume[I](query: (Contexted[I]) => Query[_, E, Seq]): (Contexted[I]) => ResultMonad[Seq[E]] =
      (i: Contexted[I]) => i.run(query(i))
  }

  def paginated[E, T, IC , R](from: Consumer[IC, Query[E, T, Seq], R]): Consumer[IC with SimplePagination, Query[E, T, Seq], R] =
    Consumer.liftInput[IC with SimplePagination].transformQuery(from){ (x: SimplePagination) => (q: Query[E, T, Seq])=>
      val afterSkip = x.skip.fold(q)(q.drop)
      x.take.fold(afterSkip)(afterSkip.take)
    }

  def simplePagination[E] = asResult(list[E])

  def asResult[E, T, IC , R](from: Consumer[IC, Query[E, T, Seq], Seq[R]]) =
    Consumer.merge(paginated(from), length)(FilterResult.apply)

  def asJson[IC, Q, R](from: Consumer[IC, Q, R])(implicit writes: Writes[R]): Consumer[IC, Q, JsValue] =
    from.mapResult(writes.writes)
}