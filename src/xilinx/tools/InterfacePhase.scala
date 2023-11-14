package hwlib.xilinx.tools

import scala.collection.mutable

import spinal.core._
import spinal.core.internals._



case class XilinxClock(name : String, reset : Option[String], clken : Option[String], freq : Option[HertzNumber]) extends SpinalTag
case class XilinxReset(name : String, clock : String, kind : ResetKind, level : Polarity) extends SpinalTag
case class XilinxClockEnable(name : String, clock : String, level : Polarity) extends SpinalTag
case class XilinxPort(name : String, isInterface : Boolean, clock : Option[String]) extends SpinalTag
class XilinxPlain extends SpinalTag

trait XilinxInterfaceType {
  def defineXilinxInterface() : String
  def isInterface : Boolean = false

  def associateClock : Boolean = false
}

class XilinxInterfacePhase extends Phase {

  def checkClock(p : Data) : Option[ClockTag] =
    p.getTag(classOf[ClockTag])
     .orElse(p.getTag(classOf[ExternalDriverTag])
              .flatMap(_.driver.getTag(classOf[ClockTag])))

  def checkReset(p : Data) : Option[ResetTag] =
    p.getTag(classOf[ResetTag])
     .orElse(p.getTag(classOf[ExternalDriverTag])
              .flatMap(_.driver.getTag(classOf[ResetTag])))

  def checkClken(p : Data) : Option[ClockEnableTag] =
    p.getTag(classOf[ClockEnableTag])
     .orElse(p.getTag(classOf[ExternalDriverTag])
              .flatMap(_.driver.getTag(classOf[ClockEnableTag])))

  def checkDomain(p : Data) : Option[ClockDomainTag] =
    p.getTag(classOf[ClockDomainTag])


  override def impl(pc : PhaseContext) = {
    val clocks = mutable.Map[ClockDomain, Data]()
    val resets = mutable.Map[ClockDomain, Data]()
    val clkens = mutable.Map[ClockDomain, Data]()
    val intfs  = mutable.Map[ClockDomain, mutable.ArrayBuffer[String]]()
    val ports  = mutable.ArrayBuffer[(Data, String, Boolean, Option[ClockDomain])]()

    for (p <- pc.topLevel.getGroupedIO(true)) {
      checkClock(p).map { tag =>
        clocks += tag.clockDomain -> p
      } orElse checkReset(p).map { tag =>
        resets += tag.clockDomain -> p
      } orElse checkClken(p).map { tag =>
        clkens += tag.clockDomain -> p
      } getOrElse {
        p match {
          case xport : XilinxInterfaceType => {
            val name = xport.defineXilinxInterface()
            val domain = if (xport.associateClock) {
              checkDomain(p).map { tag =>
                intfs.getOrElseUpdate(tag.clockDomain, mutable.ArrayBuffer[String]()) += name
                tag.clockDomain
              }
            } else {
              None
            }
            ports.append((xport, name, xport.isInterface, domain))
          }
          case port => {
            port.addTag(new XilinxPlain)
            SpinalWarning(s"Can not define xilinx interface for ${port}, reverting to plain signals")
          }
        }
      }
    }

    // Process Clocks
    for ((domain, port) <- clocks) {
      val clkName = port.getName()
      val params = mutable.ArrayBuffer[String]()
      val freq = domain.frequency match {
        case ClockDomain.FixedFrequency(freq) => {
          val freq_hz = (freq / (1 Hz)).toLong
          // TODO-lw don't specify fixed frequency here, as it may be overridden
          //  if clock generator can not produce desired frequency exactly
          // params += s"FREQ_HZ ${freq_hz}"
          Some(freq)
        }
        case _ => {
          SpinalWarning(s"Defining Clock ${port} with variable frequency!")
          None
        }
      }
      val rstName = resets.get(domain).map { rstPort =>
        val name = rstPort.getName()
        domain.config.resetKind match {
          case ASYNC => params += s"ASSOCIATED_ASYNC_RESET ${name}"
          case _ => params += s"ASSOCIATED_RESET ${name}"
        }
        name
      }
      val ckeName = clkens.get(domain).map { ckePort =>
        val name = ckePort.getName()
        params += s"ASSOCIATED_CLKEN ${name}"
        name
      }
      intfs.get(domain).map { intfList =>
        params += s"ASSOCIATED_BUSIF ${intfList.mkString(":")}"
      }

      port.addTag(XilinxClock(clkName, rstName, ckeName, freq))
      port.addAttribute(new AttributeString("X_INTERFACE_INFO",
          s"xilinx.com:signal:clock:1.0 ${clkName} CLK"))
      port.addAttribute(new AttributeString("X_INTERFACE_PARAMETER",
          params.mkString(", ")))
    }

    // Process Resets
    for ((domain, port) <- resets) {
      val rstName = port.getName()
      val polarity = domain.config.resetActiveLevel match {
        case HIGH => "ACTIVE_HIGH"
        case LOW => "ACTIVE_LOW"
      }
      val clkName = clocks.get(domain).map(_.getName()).getOrElse {
        SpinalError(s"Reset port ${rstName} has no associated clock")
      }
      port.addTag(XilinxReset(rstName, clkName, domain.config.resetKind, domain.config.resetActiveLevel))
      port.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:signal:reset:1.0 ${rstName} RST"))
      port.addAttribute(new AttributeString("X_INTERFACE_PARAMETER",
            s"POLARITY ${polarity}"))
    }

    // Process Clock Enables
    for ((domain, port) <- clkens) {
      val ckeName = port.getName()
      val polarity = domain.config.clockEnableActiveLevel match {
        case HIGH => "ACTIVE_HIGH"
        case LOW => "ACTIVE_LOW"
      }
      val clkName = clocks.get(domain).map(_.getName()).getOrElse {
        SpinalError(s"Reset port ${ckeName} has no associated clock")
      }
      port.addTag(XilinxClockEnable(ckeName, clkName, domain.config.clockEnableActiveLevel))
      port.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:signal:reset:1.0 ${ckeName} CE"))
      port.addAttribute(new AttributeString("X_INTERFACE_PARAMETER",
            s"POLARITY ${polarity}"))
    }

    // Process Ports
    for ((port, name, isIntf, mayDomain) <- ports) {
      val clkName = mayDomain.flatMap(domain => clocks.get(domain).map(_.getName()))
      port.addTag(XilinxPort(name, isIntf, clkName))
    }

  }

  override def hasNetlistImpact = true
}

