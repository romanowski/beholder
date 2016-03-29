package org.virtuslab.beholder.consumers

import org.virtuslab.beholder.context._
import org.virtuslab.beholder.filters._
import org.virtuslab.beholder.utils.AggregationUtil
import slick.lifted._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.language.higherKinds


trait FilterConsumer[E, Filter <: BeholderFilter[E, _], R] extends (FilterDefinition => Databased[R]){
  def filter: Filter
}

case class StandardConsumer[E, Filter <: BeholderFilter[E, _]](override val filter: Filter)
  extends FilterConsumer[E, Filter, FilterResult[E]]{

  def apply(filterDefinition: FilterDefinition): Databased[FilterResult[E]] ={
    filter.apply(filterDefinition).flatMap{
      query =>
        val queryAfterSkip = filterDefinition.skip.fold(query)(query.drop)
        val afterTake = filterDefinition.take.fold(queryAfterSkip)(queryAfterSkip.take)

        for{
          list <- Databased.runQuery(afterTake)
          size <- Databased.runQuery(query.length)
        } yield FilterResult(list, size)
    }
  }
}
/*


trait Consumer[-I, Q, +R] {
  def query: I => Q

  def collect(i: I)(q: Q):  R

  final def consume(input: I): R = collect(input)(query(input))
}

trait DatabasedConsumer[-I, Q, +R] extends Consumer[I, Databased[Q], Databased[R]]

object DatabasedConsumer {
  implicit def opts[IC, Q, R](consumer: DatabasedConsumer[IC, Q, R]): DConsumerOps[IC, Q, R] = new DConsumerOps(consumer)

  implicit def queryOpts[IC, E, T, R](consumer: DatabasedConsumer[IC, Query[T, E, Seq], R]): QueryConsumerOpts[E, T, IC, R] =
    new QueryConsumerOpts(consumer)
}

class DConsumerOps[IC, Q, R](baseConsumer: DatabasedConsumer[IC, Q, R]) {
  def mapResult[NR](mapper: R => NR) =
    new DatabasedConsumer[IC, Q, NR] {
      override def query = baseConsumer.query

      override def collect(i: IC)(q: Databased[Q]): Databased[NR] = baseConsumer.collect(i)(q).map(mapper)
    }


  def narrow[NIC]: DatabasedConsumer[NIC with IC, Q, R] = baseConsumer

  def mappedMerge[NIC >: IC, R1, NR](withConsumer: DatabasedConsumer[NIC, Q, R1])(mapFunc: (R, R1) => NR) = new DatabasedConsumer[IC, Q, NR] {
    override def query = baseConsumer.query

    override def collect(i: IC)(q: Databased[Q]): Databased[NR] = {

      for{
        r1 <- baseConsumer.collect(i)(q)
        r2 <- withConsumer.collect(i)(q)
      } yield mapFunc(r1, r2)
    }
  }
}

object DbConsumers{

  type DQuery[T, E] = Databased[Query[T, E, Seq]]

  def length[I, E, T, Q <: Query[_, _, Seq](q:  I => Q) = new DatabasedConsumer[I, Q, Int]{
    override def query: I => Q = q

    override def collect(i: I)(q: Databased[Q]): Databased[Int] =
      q.flatMap{
        query => Databased.runQuery(query.length)
      }
  }

  def apply[I, E, T, Q <: DQuery[T, E]](q:  I => Q) = new DatabasedConsumer[I, Q, Seq[E]]{
    override def query: I => Q = q

    override def collect(i: I)(q: Q): Databased[Seq[E]] =
      q.flatMap(Databased.runQuery)
  }
}

object FilterConsumer{
  def apply[E, T](filter: BeholderFilter[E, T]) = new Consumer[FilterDefinition, BeholderFilter[E, T], FilterResult[E]]{
    override def query: (FilterDefinition) => BeholderFilter[E, T] = filter

    override def collect(i: FilterDefinition)(q: Databased[BeholderFilter[E, T]]): Databased[FilterResult[E]] = ???
  }
}



class QueryConsumerOpts[E, T, IC, R](baseConsumer: DatabasedConsumer[IC, Query[T, E, Seq], R]) {

  /*def paginated: Consumer[SimplePagination with IC, Query[T, E, Seq], R] =
    baseConsumer.narrow[SimplePagination].filterOnContext{
    pagination => query =>
      val queryAfterSkip = pagination.skip.fold(query)(query.drop)
      pagination.take.fold(queryAfterSkip)(queryAfterSkip.take)
  }*/

 /* def length: Consumer[IC, Rep[Int], Databased[Int]]

  def list: Consumer[IC, Query]*/
}
/*
class TableAwareCollectors[E, T] extends Collectors {

  type Q = Query[T, E, Seq]

  def lenght


/*  def joinAndAggregate[A](on: Q => Query[_, (E, Option[A]), Seq]) =
    new Consumer[Any, Q, Seq[Aggregated[E, A]]] {
      override def consume[I <: Any](query: (Contexted[I]) => Q): (Contexted[I]) => Future[Seq[Aggregated[E, A]]] =
        i => i.run(on(query(i))).map(AggregationUtil.aggregateResult)
    }*/

}

trait Collectors {

 // def lenght[IC, E, T](forQuery: IC => Query[])

  object length extends Consumer[Any,Query[_, _, Seq], Int]{
    override def consume[I](query: (Contexted[I]) => Query[_, _, Seq]): (Contexted[I]) => Future[Int] =
      (i: Contexted[I]) => i.run(query(i).length)
  }

  def list[E] = new Consumer[Any,Query[_, E, Seq], Seq[E]]{
    override def consume[I](query: (Contexted[I]) => Query[_, E, Seq]): (Contexted[I]) => Future[Seq[E]] =
      (i: Contexted[I]) => i.run(query(i))
  }

  def simplePagination[E] = list[E].paginated.mappedMerge(length)(FilterResult[E])

  def forFilter[E, T](filter: LightFilter[E, T]) = new TableAwareCollectors[E, T]
}

object Collectors extends Collectors*/

*/
