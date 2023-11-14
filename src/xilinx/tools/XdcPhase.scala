package hwlib.xilinx.tools

import scala.collection.mutable

import spinal.core._
import spinal.core.internals._

import hwlib.xilinx.tools.{ProjectArtifact, ProjectConstr}


class XdcInfoItem


case class Prop(name : String, value : Seq[String], vec : Boolean) extends XdcInfoItem {
  def valueAt(mayIdx : Option[Int]) : Option[String] = {
    if (vec) {
      value.unapply(mayIdx.getOrElse(0))
    } else {
      value.unapply(0)
    }
  }
}

object Prop {
  def apply(name : String, value : String)       : Prop = Prop(name, Seq(value), false)
  def apply(name : String, values : Seq[String]) : Prop = Prop(name,     values, true)
}

object Pin {
  def apply(loc : String) = Prop("PACKAGE_PIN", Seq(loc), false)
}

object Pins {
  def apply(locs : String*) = Prop("PACKAGE_PIN", locs, true)
}

object Std {
  def apply(std : String) = Prop("IOSTANDARD", Seq(std), false)
}

object Stds {
  def apply(stds : String*) = Prop("IOSTANDARD", stds, true)
}

object Pull {
  def apply(dir : String) = Prop("PULLTYPE", Seq(dir), false)
}

object Pulls {
  def apply(dirs : String*) = Prop("PULLTYPE", dirs, true)
}


case class Clock(period : Double, duty : Double = 0.5f, phase : Double = 0.0) extends XdcInfoItem {
  def waveform = {
    val rise = period * phase
    val fall = rise + period * duty
    Seq(rise % period, fall % period)
  }
}


case class Rename(name : String) extends XdcInfoItem


case class XdcInfo(val parts : Map[Option[String], Seq[XdcInfoItem]]) extends SpinalTag {

  def before(other : XdcInfo) = merged(this.parts, other.parts)

  def after(other : XdcInfo) = merged(other.parts, this.parts)

  def merged(base : Map[Option[String], Seq[XdcInfoItem]], update : Map[Option[String], Seq[XdcInfoItem]]) : XdcInfo = {
    val mergedMap = (base.toSeq ++ update.toSeq).groupBy(_._1)
    new XdcInfo(mergedMap.map(entry => (entry._1, entry._2.map(item => item._2).flatten)))
  }

  def select(part : Option[String], indices : Seq[Int] = Seq()) : Option[XdcInfo] = {
    if (part.isEmpty && indices.isEmpty) {
      Some(this)
    } else {
      val selectedMap : Option[Map[Option[String],Seq[XdcInfoItem]]] = if (part.isEmpty) {
        Some(parts)
      } else {
        parts.get(part).map(infos => Map(None -> infos))
      }
      val slicedMap = if (indices.isEmpty) {
        selectedMap
      } else {
        selectedMap.map(_.map(p => p._1 -> p._2.map{
          case prop : Prop if prop.vec && indices.nonEmpty => {
            val values = indices.map{
              case idx if prop.value.isDefinedAt(idx) => prop.value(idx)
              case idx => SpinalError(s"Undefined value at ${idx} of ${prop}")
            }
            Prop(prop.name, values, true)
          }
          case item => item
        }))
      }
      slicedMap.map(pmap => new XdcInfo(pmap))
    }
  }

  def takeRenames(part : Option[String] = None) : (Option[Rename], XdcInfo) = {
    val mayRename = parts.get(part).flatMap(_.findLast(_.isInstanceOf[Rename]).map(_.asInstanceOf[Rename]))
    val xdcInfo = mayRename match {
      case Some(_) => {
        val withoutRename = parts(part).filter(!_.isInstanceOf[Rename])
        new XdcInfo(parts + (part -> withoutRename))
      }
      case None => this
    }
    (mayRename, xdcInfo)
  }

  def infos(part : Option[String] = None) : Option[Seq[XdcInfoItem]] = {
    parts.get(part)
  }

}

object XdcInfo {
  def apply(infos : XdcInfoItem*) : XdcInfo = new XdcInfo(Map(None -> infos))

  def multi(partItems : (String, Seq[XdcInfoItem])*) : XdcInfo = new XdcInfo(Map.from(partItems.map(p => (Some(p._1), p._2))))
}


class XdcGenerator {

  case class PortRef(name : String, idx : Option[Int] = None) {
    def ident = idx match {
      case Some(index) => s"${name}_${index}"
      case None => s"${name}"
    }
    def ref = idx match {
      case Some(index) => s"${name}[${index}]"
      case None => s"${name}"
    }
  }

  val clockInfo = mutable.Map[PortRef, Clock]()
  val portInfo = mutable.Map[PortRef, mutable.Map[String, String]]()

  def analyzePort(port : Data, nameReplace : Option[String] = None) = {
    val pname = port.getName()
    port.flattenForeach { pport =>
      pport.getTag(classOf[XdcInfo]).flatMap(_.infos()) match {
        case Some(infos) => {
          val ppname = pport.getName().replaceFirst(pname, nameReplace.getOrElse(pname))
          pport match {
            case vport : BitVector =>
              for (idx <- 0 until vport.getWidth) {
                addPortInfo(PortRef(ppname, Some(idx)), infos)
              }
            case sport : Bool => addPortInfo(PortRef(ppname, None), infos)
            case _ => SpinalError(s"XdcGenerator: Can not constrain toplevel ports of this type ${port}")
          }
        }
        case None => SpinalWarning(s"XdcGenerator: Toplevel port ${port} has no constraints!")
      }
    }
  }

  def addPortInfo(port : PortRef, infos : Seq[XdcInfoItem]) = {
    for(item <- infos) item match {
      case clock : Clock => {
        clockInfo += (port -> clock)
      }
      case prop : Prop => {
        prop.valueAt(port.idx) match {
          case Some(value) => {
            portInfo.getOrElseUpdate(port, mutable.Map[String, String]()) += (prop.name -> value)
          }
          case None => SpinalWarning(s"XdcGenerator: Toplevel port ${port.ref} got no value for Xdc property \"${prop.name}\"")
        }
      }
      case _ => ()
    }
  }

  def getXdc() : String = {
    val buf = new StringBuilder()

    for ((port, clock) <- clockInfo) {
      buf ++= s"create_clock -add -name ${port.ident} -period ${clock.period} -waveform { ${clock.waveform.mkString(" ")} } [get_ports { ${port.ref} }]\n"
    }

    for ((port, props) <- portInfo) {
      val propsStr = props.map(item => s"${item._1} \"${item._2}\"").mkString(" ")
      buf ++= s"set_property -dict { ${propsStr} } [get_ports { ${port.ref} }]\n"
    }

    buf.toString
  }

  // TODO-lw consider further constraint types
}


class XdcPhase extends Phase {

  override def impl(pc : PhaseContext) = {
    val gen = new XdcGenerator()
    for (port <- pc.topLevel.getAllIo) {
      gen.analyzePort(port)
    }
    pc.topLevel.addTag(ProjectArtifact("{{topName}}.xdc", gen.getXdc(), ProjectConstr))
  }

  override def hasNetlistImpact = false

}
