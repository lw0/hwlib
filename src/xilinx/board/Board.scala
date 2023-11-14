package hwlib.xilinx.board

import hwlib.xilinx.tools.XdcInfo


abstract class Board {
  def part : String
  def board : Option[String]
  def ports : Map[String, XdcInfo]
}

