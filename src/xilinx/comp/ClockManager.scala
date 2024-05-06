package hwlib.xilinx.comp

import scala.collection.mutable.Set

import spinal.core._
import spinal.core.sim._
import spinal.core.internals._

import hwlib.xilinx.blackbox.{ClkManPrimitive, Part, ClkBufGlobal, ClockSpec, Pll2Base, Mmcm2Base, ClockOptTarget, ClkManCfg, ClkBufPrimitive, CdcReset, CdcResetConfig}





class ClockManager(part : Part, specs : Map[String, ClockSpec], usePll : Boolean = false, baseCfg : ClkManCfg = ClkManCfg(), optFor : Seq[ClockOptTarget] = Seq()) extends Component {

  val freqIn = ClockDomain.current.frequency match {
    case ClockDomain.FixedFrequency(freq) => freq
    case _ => SpinalError(s"ClockManager requires frequency info from input clock domain ${ClockDomain.current}")
  }

  val kind = if (usePll) { Pll2Base } else { Mmcm2Base }
  val mayParam = part.clkManBounds(kind).optimize(freqIn, specs, optFor)
  val param = mayParam match {
    case Some(param) if !param.canFit => SpinalError(s"ClockManager can not generate as many as ${param.order.length} different clocks")
    case Some(param) => {
      SpinalInfo(s"ClockManager Parameters:\n${param.desc}")
      param
    }
    case None => {
      val fIn = freqIn.decompose
      val fOutStr = specs.map(e => f"${e._2.freq.decompose._1}%.3f ${e._2.freq.decompose._2}%s").mkString(", ")
      SpinalError(f"ClockManager could not find valid parameters for freqIn = ${fIn._1}%.3f ${fIn._2}%s and freqOut = (${fOutStr}%s)")
    }
  }
  val outCount = param.order.length

  val io = new Bundle {
    val oClk = Vec(out(Bool), outCount)
    val oRst = Vec(out(Bool), outCount)
  }


  val iClkMan = new ClkManPrimitive(kind, param.getCfg(baseCfg))
  iClkMan.io.CLKIN1 := ClockDomain.current.readClockWire
  iClkMan.io.RST := ClockDomain.current.isResetActive

  val iClkBufFb = new ClkBufPrimitive(ClkBufGlobal)
  iClkBufFb.io.I := iClkMan.io.CLKFBOUT
  iClkMan.io.CLKFBIN := iClkBufFb.io.O

  val sReset = !iClkMan.io.LOCKED || ClockDomain.current.isResetActive

  val iOutLogic = for (idx <- 0 until outCount) yield {
    val sClk = iClkMan.io.find(s"CLKOUT${idx}").asInstanceOf[Bool]
    val iRstSync = new CdcReset(CdcResetConfig(async = true, activeHigh = true))
    iRstSync.io.src_rst := sReset
    iRstSync.io.dest_clk := sClk
    io.oClk(idx) := sClk
    io.oRst(idx) := iRstSync.io.dest_rst
  }

  val clockDomains = param.mapping.map(e =>
    e._1 -> ClockDomain(
              clock = io.oClk(e._2),
              reset = io.oRst(e._2),
              frequency = ClockDomain.FixedFrequency(param.order(e._2).freq(param.fractOutput.map(_ == e._1).getOrElse(false))),
              config = ClockDomainConfig(RISING, ASYNC, HIGH)))
}
