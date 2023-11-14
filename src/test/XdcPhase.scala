package hwlib.test

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

import scala.collection.mutable.{Map}

import spinal.core._
import spinal.core.sim._
import spinal.core.internals._
import spinal.lib._

import hwlib.xilinx.tools.{XdcInfo, Std, Stds, Rename, BoardRef, BoardRefPhase, Clock, ProjectGenerator, XdcPhase}
import hwlib.xilinx.board.Nexys4DDR
import hwlib.serial.{TSerial, SerialConfig}
import hwlib.HwMain



import BoardRef.singularRange

class TestXdc extends Component {

  val serCfg = SerialConfig(hasHandshake=true)

  val io = new Bundle {
    val i_clk    =     in(Bool)             addTag(BoardRef("CLK100M"))
    val i_rst_n  =     in(Bool)             addTag(BoardRef("RST_N"))
    val o_bit    =    out(Bool)             addTag(BoardRef("RGBL", "green"))
    val o_vec    =    out(UInt(4 bits))     addTag(BoardRef("LED", 2 to 3, 8)) addTag(XdcInfo(Stds("LVCMOS33", "LVCMOS18", "LVCMOS18", "LVCMOS33")))
    val o_dummy  =    out(UInt(3 bits))
    val i_bit    =     in(Bool)             addTag(BoardRef("SWITCH", 3))
    val i_vec    =     in(Bits(2 bits))     addTag(BoardRef("SWITCH", 0 to 1))
    val m_serial =  slave(TSerial(serCfg))  addTag(BoardRef("SERIAL")) addTag(XdcInfo.multi("rts"->Seq(Std("LVCMOS18")), "cts"->Seq(Rename("lucy"))))
  }

  val rootClock = ClockDomain(
    clock = io.i_clk,
    reset = io.i_rst_n,
    frequency = FixedFrequency(100 MHz),
    config = ClockDomainConfig(
      clockEdge = RISING,
      resetKind = ASYNC,
      resetActiveLevel = LOW))

  val rootArea = new ClockingArea(rootClock) {
    io.o_bit := io.i_bit
    io.o_vec := (io.i_vec ## io.i_vec).asUInt

    io.o_dummy := 0

    io.m_serial.rx := io.m_serial.tx
    io.m_serial.cts := io.m_serial.rts
  }

  noIoPrefix()
  addTag(noNumericType)
}

object TestXdc extends HwMain[TestXdc] {

  genWith(new TestXdc)

  genEarlyPhase(new BoardRefPhase(Nexys4DDR))
  genEarlyPhase(new XdcPhase)

  postGen{ (args, report) =>
    report.printPruned()
    println(" ===== Specs =====")
    val projGen = new ProjectGenerator(report, Nexys4DDR)
    projGen.generate()
  }

}
