package hwlib.xilinx.tools

import spinal.core._
import spinal.core.internals._

import hwlib.xilinx.board.Board


case class BoardRef(name : String, part : Option[String], indexSpec : Seq[Range]) extends SpinalTag {

  override def toString() : String = {
    val partStr = part.map(str => s".\"${str}\"").getOrElse("")
    val idxRangeStrs = indexSpec.collect{
      case r if r.length == 1 => s"${r.start}"
      case r if r.length > 1 => s"${r.start}..${r.end}"
    }
    val idxStr =
      if (idxRangeStrs.length > 0) { s"{${idxRangeStrs.mkString(",")}}" }
      else { "" }
    s"BoardRef(\"${name}\"${partStr}${idxStr})"
  }

  def indices = indexSpec.flatten

}

object BoardRef {
  implicit def singularRange(item : Int) : Range = item to item

  def apply(name : String) : BoardRef                               = BoardRef(name, None, Seq())
  def apply(name : String, bits : Range*) : BoardRef                = BoardRef(name, None, bits)
  def apply(name : String, part : String) : BoardRef                = BoardRef(name, Some(part), Seq())
  def apply(name : String, part : String, bits : Range*) : BoardRef = BoardRef(name, Some(part), bits)

}


class BoardRefPhase(val board : Board) extends Phase {

  def resolve(boardRef : BoardRef) : Option[XdcInfo] = {
    board.ports.get(boardRef.name).flatMap(_.select(boardRef.part, boardRef.indices))
  }

  def foreachPortInfo(pc : PhaseContext)(callback : (Data, Option[XdcInfo], Option[BoardRef], Option[XdcInfo]) => Unit) : Unit = {
    for (p <- pc.topLevel.getGroupedIO(true)) {
      val xdcInfo = p.getTag(classOf[XdcInfo])
      val boardRef = p.getTag(classOf[BoardRef])
      val boardInfo = boardRef.flatMap(resolve(_))

      if (xdcInfo.isDefined) {
        p.removeTag(xdcInfo.get)
      }
      if (boardRef.isDefined) {
        p.removeTag(boardRef.get)
      }

      if (p.isInstanceOf[MultiData]) {
        p.asInstanceOf[MultiData].flattenForeach{
          sp =>
            if (xdcInfo.isDefined) {
              sp.removeTag(xdcInfo.get)
            }
            if (boardRef.isDefined) {
              sp.removeTag(boardRef.get)
            }

            val part = sp.getPartialName()
            val partXdcInfo = xdcInfo.flatMap(_.select(Some(part)))
            val partBoardRef = boardRef.map(_.copy(part=Some(part)))
            val partBoardInfo = boardInfo.flatMap(_.select(Some(part)))
            callback(sp, partXdcInfo, partBoardRef, partBoardInfo)
        }
      } else {
        callback(p, xdcInfo, boardRef, boardInfo)
      }
    }
  }

  override def impl(pc : PhaseContext) = {
    foreachPortInfo(pc) {
      (port, x, r, i) =>
        (r, i) match {
          case (Some(boardRef), Some(boardInfo)) => {
            // println(s"Port ${port.getName()} : ${port.getClass().getName}  ${x} ${boardRef}: ${boardInfo.infos()}")
            val preInfo = x match {
              case Some(xdcInfo) => xdcInfo.after(boardInfo)
              case None => boardInfo
            }
            val newInfo = preInfo.takeRenames() match {
              case (Some(rename), info) => port.setName(rename.name); info
              case (None, info) => info
            }
            port.addTag(newInfo)
          }
          case (Some(boardRef), None) =>
            SpinalError(s"Port ${port.getName()}: ${boardRef} can not be resolved! ")
          case (None, _) => ()
        }
    }
  }

  override def hasNetlistImpact = false
}


