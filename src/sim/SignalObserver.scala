package hwlib.sim

import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.math._

import spinal.core._
import spinal.sim._
import spinal.core.sim._


class SignalObserver(signal : Bits)(implicit val env : ClockEnv) {

  case class Matcher(
      maskTrue : BigInt,
      maskFalse : BigInt,
      maskRise : BigInt,
      maskFall : BigInt,
      condAnd : Boolean,
      call : Option[BigInt] => Boolean)

  val matchers = ListBuffer[Matcher]()

  var lastTrue = BigInt(0)
  var lastValid = false

  env.onClk{ () =>
    val stateTrue = signal.toBigInt
    val stateFalse = ~stateTrue
    val stateRise = if (lastValid) {
      ~lastTrue & stateTrue
    } else {
      BigInt(0)
    }
    val stateFall = if (lastValid) {
      lastTrue & ~stateTrue
    } else {
      BigInt(0)
    }
    lastTrue = stateTrue
    lastValid = true

    matchers.filterInPlace{ m =>
      val matchTrue = if (m.condAnd) {
        (stateTrue & m.maskTrue) == m.maskTrue
      } else {
        (stateTrue & m.maskTrue) != 0
      }
      val matchFalse = if (m.condAnd) {
        (stateFalse & m.maskFalse) == m.maskFalse
      } else {
        (stateFalse & m.maskFalse) != 0
      }
      val matchRise = if (m.condAnd) {
        (stateRise & m.maskRise) == m.maskRise
      } else {
        (stateRise & m.maskRise) != 0
      }
      val matchFall = if (m.condAnd) {
        (stateFall & m.maskFall) == m.maskFall
      } else {
        (stateFall & m.maskFall) != 0
      }
      val matchAll = if (m.condAnd) {
        (matchTrue  || m.maskTrue == 0) &&
        (matchFalse || m.maskFalse == 0) &&
        (matchRise  || m.maskRise == 0) &&
        (matchFall  || m.maskFall == 0)
      } else {
        matchTrue || matchFalse || matchRise || matchFall
      }
      if (matchAll) {
        m.call(Some(stateTrue))
      } else {
        true
      }
    }
  }

  env.onRst{ () =>
    lastValid = false
    matchers.filterInPlace{ m =>
      m.call(None)
    }
  }

  def trigOr(
      maskTrue : BigInt = 0,
      maskFalse : BigInt = 0,
      maskRise : BigInt = 0,
      maskFall : BigInt = 0) : Future[Option[BigInt]] = {
    val fut = Future[Option[BigInt]]()
    matchers.addOne(Matcher(maskTrue, maskFalse, maskRise, maskFall, false, (value) => { fut.resolve(value); false }))
    fut
  }

  def trigAnd(
      maskTrue : BigInt = 0,
      maskFalse : BigInt = 0,
      maskRise : BigInt = 0,
      maskFall : BigInt = 0) : Future[Option[BigInt]] = {
    val fut = Future[Option[BigInt]]()
    matchers.addOne(Matcher(maskTrue, maskFalse, maskRise, maskFall, true, (value) => { fut.resolve(value); false }))
    fut
  }

  def waitOr(
      maskTrue : BigInt = 0,
      maskFalse : BigInt = 0,
      maskRise : BigInt = 0,
      maskFall : BigInt = 0) : Option[BigInt] = {
    trigOr(maskTrue, maskFalse, maskRise, maskFall).blockValue()
  }

  def waitAnd(
      maskTrue : BigInt = 0,
      maskFalse : BigInt = 0,
      maskRise : BigInt = 0,
      maskFall : BigInt = 0) : Option[BigInt] = {
    trigAnd(maskTrue, maskFalse, maskRise, maskFall).blockValue()
  }

  def onAndWhile(
      maskTrue : BigInt = 0,
      maskFalse : BigInt = 0,
      maskRise : BigInt = 0,
      maskFall : BigInt = 0)(call : Option[BigInt] => Boolean) : Unit = {
    matchers.addOne(Matcher(maskTrue, maskFalse, maskRise, maskFall, true, call))
  }

  def onOrWhile(
      maskTrue : BigInt = 0,
      maskFalse : BigInt = 0,
      maskRise : BigInt = 0,
      maskFall : BigInt = 0)(call : Option[BigInt] => Boolean) : Unit = {
    matchers.addOne(Matcher(maskTrue, maskFalse, maskRise, maskFall, false, call))
  }

  def onAndOnce(
      maskTrue : BigInt = 0,
      maskFalse : BigInt = 0,
      maskRise : BigInt = 0,
      maskFall : BigInt = 0)(call : Option[BigInt] => Unit) : Unit = {
    matchers.addOne(Matcher(maskTrue, maskFalse, maskRise, maskFall, true, (value) => { call(value); false }))
  }

  def onOrOnce(
      maskTrue : BigInt = 0,
      maskFalse : BigInt = 0,
      maskRise : BigInt = 0,
      maskFall : BigInt = 0)(call : Option[BigInt] => Unit) : Unit = {
    matchers.addOne(Matcher(maskTrue, maskFalse, maskRise, maskFall, false, (value) => { call(value); false }))
  }

  def onAnd(
      maskTrue : BigInt = 0,
      maskFalse : BigInt = 0,
      maskRise : BigInt = 0,
      maskFall : BigInt = 0)(call : Option[BigInt] => Unit) : Unit = {
    matchers.addOne(Matcher(maskTrue, maskFalse, maskRise, maskFall, true, (value) => { call(value); true }))
  }

  def onOr(
      maskTrue : BigInt = 0,
      maskFalse : BigInt = 0,
      maskRise : BigInt = 0,
      maskFall : BigInt = 0)(call : Option[BigInt] => Unit) : Unit = {
    matchers.addOne(Matcher(maskTrue, maskFalse, maskRise, maskFall, false, (value) => { call(value); true }))
  }
}
