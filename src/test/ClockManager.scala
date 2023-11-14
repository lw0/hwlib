package hwlib.test

import spinal.core._
import spinal.core.sim._
import spinal.core.internals._
import spinal.lib._

import hwlib.xilinx.comp.ClockManager
import hwlib.xilinx.blackbox.{Artix7Speed1, ClockSpec}
import hwlib.sim._
import hwlib.HwMain



class StaticMmcm extends Component {

  val io = new Bundle {
    val oCountAlpha  = out(UInt(4 bits))
    val oCountBeta  = out(UInt(4 bits))
    val oCountGamma  = out(UInt(4 bits))
    val oCountDelta  = out(UInt(4 bits))
  }

  val iClockManager = new ClockManager(Artix7Speed1, Map(
        "alpha" -> ClockSpec(108 MHz),
        "beta"  -> ClockSpec( 54 MHz, phase=0.625, duty=0.333),
        "gamma" -> ClockSpec(216 MHz, phase=0.25),
        "delta" -> ClockSpec(108 MHz, phase=0.25)))

  val iClockAreaAlpha = new ClockingArea(iClockManager.clockDomains("alpha")) {
    val sCounter = Counter(4 bits)
    sCounter.increment()
    io.oCountAlpha := sCounter
  }

  val iClockAreaBeta = new ClockingArea(iClockManager.clockDomains("beta")) {
    val sCounter = Counter(4 bits)
    sCounter.increment()
    io.oCountBeta := sCounter
  }

  val iClockAreaGamma = new ClockingArea(iClockManager.clockDomains("gamma")) {
    val sCounter = Counter(4 bits)
    sCounter.increment()
    io.oCountGamma := sCounter
  }

  val iClockAreaDelta = new ClockingArea(iClockManager.clockDomains("delta")) {
    val sCounter = Counter(4 bits)
    sCounter.increment()
    io.oCountDelta := sCounter
  }
}

object StaticMmcm extends HwMain[StaticMmcm] {

  genWith(new StaticMmcm)
  postGen{ (args, report) =>
    report.printPruned()
  }

  simWith(new StaticMmcm)
  clockAt(100 MHz)
  simRun{ dut =>
    implicit val env = new ClockEnv(dut.clockDomain)

    env.onRawCycle(100) { () =>
     env.setRst(false)
    }

    env.waitFor(10000)

  }
}
