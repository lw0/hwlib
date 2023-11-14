package hwlib.sim

import scala.collection.mutable.{PriorityQueue, ListBuffer}
import scala.math._

import spinal.core._
import spinal.sim._
import spinal.core.sim._


class ClockEnv(val clk : ClockDomain) {

  var dbg = new Debug(this)

  var cycles : Long = 0 // honours rst and cke
  var cyclesRaw : Long = 0 // ignores rst and cke

  case class AtCall(val at : Long, val callback : () => Unit) extends Ordered[AtCall] {
    def compare(that : AtCall) = {signum(that.at - this.at).toInt}
  }

  case class Call(val cond : () => Boolean, val callback : () => Boolean)

  private val actionsAt = new PriorityQueue[AtCall]()
  private val actionsAtRaw = new PriorityQueue[AtCall]()
  private val actionsClk = new ListBuffer[Call]()
  private val actionsClkRaw = new ListBuffer[Call]()
  private val actionsRst = new ListBuffer[Call]()

  private var wantsRst : Option[Boolean] = Some(true)
  private var wantsCke : Option[Boolean] = Some(true)

  private var lastRst : Boolean = false

  clk.frequency match {
    case ClockDomain.FixedFrequency(freq) =>
      val ticks = (freq.toTime / (1 ps)).toLong
      if (ticks >= 4) {
        fork {
          val tTock = ticks / 2
          val tTick1 = 1
          val tTickN = ticks - tTock - 1
          applyClk(false)
          applyCke()
          applyRst()
          while (true) {
            sleep(tTick1)
            applyRst()
            applyCke()
            sleep(tTickN)
            applyClk(false)
            sleep(tTock)
            applyClk(true)
          }
        }
      } else {
        SpinalError(s"ClockEnv(${clk}): Frequency ${freq} is too fast for auto-toggling!")
      }
    case _ => ()
  }

  clk.onActiveEdges {
    cyclesRaw += 1
    if (clk.isResetAsserted) {
      cycles = 0
    } else if (clk.isClockEnableAsserted) {
      cycles += 1
    }

    doActionsAtRaw()
    doActionsClkRaw()

    if (clk.isSamplingEnable) {
      doActionsAt()
      doActionsClk()
    }

    if (!lastRst && clk.isResetAsserted) {
      doActionsRst()
    }
    lastRst = clk.isResetAsserted
  }

  def setRst(assert : Boolean) : Unit = {
    wantsRst = Some(assert)
  }

  def setCke(assert : Boolean) : Unit = {
    wantsCke = Some(assert)
  }


  def timeout(cycles : Long) : Unit = {
    onRawCycle(cycles)(() => simFailure("Timeout"))
  }


  def trigAfter(delay : Long = 0L) : Future[Long] = {
    val fut = Future[Long]()
    onCycle(cycles + delay)(() => fut.resolve(cycles))
    fut
  }
  def waitFor(delay : Long = 0L) : Long = {
    trigAfter(delay).blockValue()
  }

  def trigAt(cycle : Long) : Future[Long] = {
    val fut = Future[Long]()
    onCycle(cycle)(() => fut.resolve(cycles))
    fut
  }
  def waitUntil(cycle : Long) : Long = {
    trigAt(cycle).blockValue()
  }

  def trigAfterRaw(delay : Long = 0L) : Future[Long] = {
    val fut = Future[Long]()
    onRawCycle(cycles + delay)(() => fut.resolve(cycles))
    fut
  }
  def waitForRaw(delay : Long = 0L) : Long = {
    trigAfterRaw(delay).blockValue()
  }

  def trigAtRaw(cycle : Long) : Future[Long] = {
    val fut = Future[Long]()
    onRawCycle(cycle)(() => fut.resolve(cycles))
    fut
  }
  def waitUntilRaw(cycle : Long) : Long = {
    trigAtRaw(cycle).blockValue()
  }


  def onCycle(cycle : Long)(action : () => Unit) = {
    actionsAt.enqueue(AtCall(cycle, action))
  }

  def onRawCycle(cycle : Long)(action : () => Unit) = {
    actionsAtRaw.enqueue(AtCall(cycle, action))
  }


  def onClk(action : () => Unit) = {
    actionsClk.append(Call(() => true, () => {action(); false}))
  }

  def onRawClk(action : () => Unit) = {
    actionsClkRaw.append(Call(() => true, () => {action(); false}))
  }
  def onClkWhen(cond : () => Boolean)(action : () => Unit) = {
    actionsClk.append(Call(cond, () => {action(); false}))
  }

  def onRawClkWhen(cond : () => Boolean)(action : () => Unit) = {
    actionsClkRaw.append(Call(cond, () => {action(); false}))
  }

  def onNextClk(action : () => Unit) = {
    actionsClk.append(Call(() => true, () => {action(); true}))
  }

  def onNextRawClk(action : () => Unit) = {
    actionsClkRaw.append(Call(() => true, () => {action(); true}))
  }

  def onNextClkWhen(cond : () => Boolean)(action : () => Unit) = {
    actionsClk.append(Call(cond, () => {action(); true}))
  }

  def onNextRawClkWhen(cond : () => Boolean)(action : () => Unit) = {
    actionsClkRaw.append(Call(cond, () => {action(); true}))
  }

  def onClkUntil(action : () => Boolean) = {
    actionsClk.append(Call(() => true, action))
  }

  def onRawClkUntil(action : () => Boolean) = {
    actionsClkRaw.append(Call(() => true, action))
  }

  def onClkWhenUntil(cond : () => Boolean)(action : () => Boolean) = {
    actionsClk.append(Call(cond, action))
  }

  def onRawClkWhenUntil(cond : () => Boolean)(action : () => Boolean) = {
    actionsClkRaw.append(Call(cond, action))
  }


  def trigNextWhen(cond : => Boolean) : Future[Unit] = {
    val fut = Future[Unit]()
    onNextClkWhen(() => cond)(() => fut.resolve())
    fut
  }
  def waitNextWhen(cond : => Boolean) = {
    trigNextWhen(cond).blockValue()
  }

  def trigNextWhenRaw(cond : => Boolean) : Future[Unit] = {
    val fut = Future[Unit]()
    onNextRawClkWhen(() => cond)(() => fut.resolve())
    fut
  }
  def waitNextWhenRaw(cond : => Boolean) = {
    trigNextWhenRaw(cond).blockValue()
  }


  def onRst(action : () => Unit) = {
    actionsRst.append(Call(() => true, () => {action(); false}))
  }

  def onRstWhen(cond : () => Boolean)(action : () => Unit) = {
    actionsRst.append(Call(cond, () => {action(); false}))
  }

  def onNextRst(action : () => Unit) = {
    actionsRst.append(Call(() => true, () => {action(); true}))
  }

  def onNextRstWhen(cond : () => Boolean)(action : () => Unit) = {
    actionsRst.append(Call(cond, () => {action(); true}))
  }

  def onRstUntil(action : () => Boolean) = {
    actionsRst.append(Call(() => true, action))
  }

  def onRstWhenUntil(cond : () => Boolean)(action : () => Boolean) = {
    actionsRst.append(Call(cond, action))
  }


  private def applyRst() : Unit = {
    if (wantsRst.isDefined) {
      val assert = wantsRst.get
      wantsRst = None
      if (clk.hasResetSignal) {
        if (assert) clk.assertReset() else clk.deassertReset()
      }
    }
  }

  private def applyCke() : Unit = {
    if (wantsCke.isDefined) {
      val assert = wantsCke.get
      wantsCke = None
      if (clk.hasClockEnableSignal) {
        if (assert) clk.assertClockEnable() else clk.deassertClockEnable()
      }
    }
  }

  private def applyClk(edge : Boolean) : Unit = {
    if ((clk.config.clockEdge == RISING) == edge) {
      clk.risingEdge()
    } else {
      clk.fallingEdge()
    }
  }

  private def doActionsAtRaw() = {
    while (actionsAtRaw.nonEmpty && (actionsAtRaw.head.at <= cyclesRaw)) {
      actionsAtRaw.dequeue().callback()
    }
  }

  private def doActionsAt() = {
    while (actionsAt.nonEmpty && (actionsAt.head.at <= cycles)) {
      actionsAt.dequeue().callback()
    }
  }

  private def doActionsClk() = {
    actionsClk.filterInPlace{
      (el) =>
        if (el.cond()) {
          !el.callback()
        } else {
          true
        }
    }
  }

  private def doActionsClkRaw() = {
    actionsClkRaw.filterInPlace{
      (el) =>
        if (el.cond()) {
          !el.callback()
        } else {
          true
        }
    }
  }

  private def doActionsRst() = {
    actionsRst.filterInPlace{
      (el) =>
        if (el.cond()) {
          !el.callback()
        } else {
          true
        }
    }
  }
}


