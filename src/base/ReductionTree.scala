package hwlib.base

import scala.collection.mutable

import spinal.core._


case class ReductionTree(val inputWidths : Seq[Int], emptyStages : Boolean = false) {
  require(inputWidths.nonEmpty)
  val inputCount = inputWidths.length

  // TODO-lw add reordering option to minimize total data width ?
  case class Cell(level : Int, portA : Int, portB : Option[Int], portOut : Int, portWidth : Int)

  val levelCount = log2Up(inputCount)
  val portIn : Seq[Int] = 0 until inputCount

  private var portWidths = mutable.ArrayBuffer.from(inputWidths)
  private val cellList = mutable.ArrayBuffer[Cell]()
  private val outMap = mutable.ArrayBuffer[Seq[Int]]()

  for (iStage <- 0 until levelCount) {
    val prevOutput = if (iStage == 0) {
      portIn
    } else {
      outMap(iStage - 1)
    }
    val cellCount = (prevOutput.length - 1) / 2 + 1
    val stage = for (iCell <- 0 until cellCount) yield {
      val idxA = 2 * iCell
      val idxB = 2 * iCell + 1
      if (idxB < prevOutput.length) {
        val width = portWidths(prevOutput(idxA)) max portWidths(prevOutput(idxB))
        val portIdx = portWidths.length
        portWidths += width
        cellList += Cell(iStage, prevOutput(idxA), Some(prevOutput(idxB)), portIdx, width)
        portIdx
      } else if (emptyStages) {
        val width = portWidths(prevOutput(idxA))
        val portIdx = portWidths.length
        portWidths += width
        cellList += Cell(iStage, prevOutput(idxA), None, portIdx, width)
        portIdx
      } else {
        prevOutput(idxA)
      }
    }
    outMap += stage
  }

  def portWidth(idx : Int) = portWidths(idx)
  val portOut = portWidths.length - 1
  def portCount = portWidths.length
  def cells = cellList.toSeq
}


