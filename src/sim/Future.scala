package hwlib.sim

import scala.collection.mutable.{Queue, Set}

import spinal.core.sim.simThread


abstract class AbstractFuture {
  def isResolved : Boolean
  def onResolve(call : (AbstractFuture) => Unit) : Unit
  def cancelResolve(call : (AbstractFuture) => Unit) : Unit
  def block() : Unit
}

case class Future[T](init : Option[T] = None) extends AbstractFuture {

  private val waiters = Queue[(AbstractFuture) => Unit]()
  var value : Option[T] = init

  def resolve(v : T) : Future[T] = {
    require(value.isEmpty, s"Can not reassign resolved future")
    value = Some(v)
    waiters.dropWhileInPlace {
      (callback) =>
        callback(this)
        true
    }
    this
  }

  def isResolved = value.isDefined

  def onResolve(call : (AbstractFuture) => Unit) : Unit = {
    if (value.isEmpty) {
      waiters.enqueue(call)
    } else {
      call(this)
    }
  }

  def cancelResolve(call : (AbstractFuture) => Unit) : Unit = {
    waiters.subtractOne(call)
  }

  def block() : Unit = {
    while (value.isEmpty) {
      val t = simThread
      waiters.enqueue((fut) => t.resume())
      t.suspend()
    }
  }

  def blockValue() : T = {
    block()
    value.get
  }

}

class FutureSet(val any : Boolean, futs : Set[AbstractFuture]) extends AbstractFuture {

  val waiters = Queue[(AbstractFuture) => Unit]()

  val futures = Set.from(futs)
  futs.foreach((fut) => fut.onResolve(didResolve))

  def isResolved : Boolean = futures.isEmpty

  def onResolve(call : (AbstractFuture) => Unit) : Unit = {
    if (isResolved) {
      call(this)
    } else {
      waiters.enqueue(call)
    }
  }

  def cancelResolve(call : (AbstractFuture) => Unit) : Unit = {
    waiters.subtractOne(call)
  }

  def block() : Unit = {
    if (futures.nonEmpty) {
      val t = simThread
      waiters.enqueue((fut) => t.resume())
      t.suspend()
    }
  }

  def didResolve(fut : AbstractFuture) : Unit = {
    if (futures.remove(fut) && any || futures.isEmpty) {
      resolve()
    }
  }

  private def resolve() : Unit = {
    futures.filterInPlace {
      (fut) =>
        fut.cancelResolve(didResolve)
        false
    }
    waiters.dropWhileInPlace {
      (callback) =>
        callback(this)
        true
    }
  }

}

object FutureSet {
  case class FutureSetBuilder(val any : Boolean) {
    val futures = Set[AbstractFuture]()

    def add(fut : AbstractFuture) = {
      futures.add(fut)
      this
    }

   def add(futs : IterableOnce[AbstractFuture]) = {
     futures.addAll(futs)
     this
   }

   def get() = {
     new FutureSet(any, futures)
   }
  }

  def All = FutureSetBuilder(false)
  def Any = FutureSetBuilder(true)
}
