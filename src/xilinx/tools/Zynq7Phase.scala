package hwlib.xilinx.tools

import scala.collection.mutable

import spinal.core._
import spinal.core.internals._

import hwlib.amba.AxiConfig
import hwlib.serial.SerialConfig


case class Zynq7AxiPort(
            name : String,
            isMaster : Boolean,
            direct : Boolean,
            props : Seq[(String, String)],
            mapAt : Option[(Long, Option[Long])] = None) extends SpinalTag

case class Zynq7IrqPort(idx : Int) extends SpinalTag
case class Zynq7MioPort(port : String, props : Seq[(String, String)]) extends SpinalTag

object Zynq7Port {

  val CfgSAxiGP      = AxiConfig(addrBits = 32, dataBytes = 4, idBits = 6, isAxi3 = true,
                                 hasLock = true, hasCache = true, hasProt = true, hasQos = true)

  val CfgSAxiHP      = AxiConfig(addrBits = 32, dataBytes = 4, idBits = 6, isAxi3 = true,
                                 hasLock = true, hasCache = true, hasProt = true, hasQos = true)
  val CfgSAxiHPWide  = AxiConfig(addrBits = 32, dataBytes = 8, idBits = 6, isAxi3 = true,
                                 hasLock = true, hasCache = true, hasProt = true, hasQos = true)

  val CfgSAxiACP     = AxiConfig(addrBits = 32, dataBytes = 8, idBits = 3, isAxi3 = true,
                                 hasLock = true, hasCache = true, hasProt = true, hasQos = true)
  val CfgSAxiACPUser = AxiConfig(addrBits = 32, dataBytes = 8, idBits = 3, isAxi3 = true, aUserBits = 5,
                                 hasLock = true, hasCache = true, hasProt = true, hasQos = true)

  val CfgMAxiGP      = AxiConfig(addrBits = 32, dataBytes = 4, idBits = 12, isAxi3 = true,
                                 hasLock = true, hasCache = true, hasProt = true, hasQos = true)
  val CfgMAxiGPRemap = AxiConfig(addrBits = 32, dataBytes = 4, idBits = 6, isAxi3 = true,
                                 hasLock = true, hasCache = true, hasProt = true, hasQos = true)

  val CfgSerial   = SerialConfig(hasHandshake=true, negHandshake=true,
                                 hasPresence=true, negPresence=true,
                                 hasModem=true, negCarrier=true, negRing=true)

  def SAxiGP(idx : Int,
          direct : Boolean = false) = {
    if (0 <= idx && idx < 2) {
      // val native = CfgSAxiGP
      new Zynq7AxiPort(s"S_AXI_GP${idx}", false, direct, Seq())
    } else {
      SpinalError(s"Zynq7AxiPort: S_AXI_GP${idx} does not exist")
    }
  }

  def SAxiACP(
          direct : Boolean = false,
          user : Boolean = false,
          check : Boolean = false) = {
    val propAxUser = "PCW_USE_DEFAULT_ACP_USER_VAL" -> (if (user) {"{0}"} else {"{1}"})
    val propChecker = "PCW_INCLUDE_ACP_TRANS_CHECK" -> (if (check) {"{1}"} else {"{0}"})
    // val native = (if (user) {CfgSAxiACPUser} else {CfgSAxiACP})

    new Zynq7AxiPort("S_AXI_ACP", false, direct, Seq(propAxUser, propChecker))
  }

  def SAxiHP(idx : Int,
          direct : Boolean = false,
          wide : Boolean = false) = {
    if (0 <= idx && idx < 4) {
      val propWide = s"PCW_S_AXI_HP${idx}_DATA_WIDTH" -> (if (wide) {"{64}"} else {"{32}"})
      // val native = (if (wide) {CfgSAxiHPWide} else {CfgSAxiHP})

      new Zynq7AxiPort(s"S_AXI_HP${idx}", false, direct, Seq(propWide))
    } else {
      SpinalError(s"Zynq7AxiPort: S_AXI_HP${idx} does not exist")
    }
  }

  def _MAxiGP(idx : Int,
          mapAt : Option[(Long, Option[Long])] = None,
          direct : Boolean,
          remap : Boolean,
          secure : Boolean) : Zynq7AxiPort = {
    if (0 <= idx && idx < 2) {
      val propRemap = s"PCW_M_AXI_GP${idx}_ENABLE_STATIC_REMAP" -> (if (remap) {"{1}"} else {"{0}"})
      val propSecure = "PCW_USE_AXI_NONSECURE" -> (if (secure) {"{0}"} else {"{1}"})
      // val native = (if (remap) {CfgMAxiGPRemap} else {CfgMAxiGP})

      new Zynq7AxiPort(s"M_AXI_GP${idx}", true, direct, Seq(propRemap, propSecure), mapAt)
    } else {
      SpinalError(s"Zynq7AxiPort: S_AXI_HP${idx} does not exist")
    }
  }
  def MAxiGP(idx : Int,
          direct : Boolean = false,
          remap : Boolean = false,
          secure : Boolean = false) = {
    _MAxiGP(idx, None, direct, remap, secure)
  }
  def MAxiGPAt(idx : Int,
          mapAt : Long,
          direct : Boolean = false,
          remap : Boolean = false,
          secure : Boolean = false) = {
    _MAxiGP(idx, Some((mapAt, None)), direct, remap, secure)
  }
  def MAxiGPRange(idx : Int,
          mapAt : Long,
          mapSize : Long,
          direct : Boolean = false,
          remap : Boolean = false,
          secure : Boolean = false) = {
    _MAxiGP(idx, Some((mapAt, Some(mapSize))), direct, remap, secure)
  }

  def Irq(idx : Int) = new Zynq7IrqPort(idx)

  def Mio(port : String, props : (String,String)*) =
        new Zynq7MioPort(port, Seq.from(props))
  def Uart(idx : Int, fullSignals : Boolean = false) =
        new Zynq7MioPort(s"UART_${idx}", Seq(
              s"PCW_UART${idx}_PERIPHERAL_ENABLE" -> "{1}",
              s"PCW_UART${idx}_GRP_FULL_ENABLE" -> (if(fullSignals){"1"}else{"0"})))

  def Clock(clockDomain : ClockDomain) = new ClockDomainTag(clockDomain)
  def CurrentClock = new ClockDomainTag(ClockDomain.current)

}

class Zynq7Phase extends Phase {

    class DesignSpecs(val nameModule : String) {

    case class ClockInfo(
      modulePort : String)
    case class ResetInfo(
      modulePort : Option[String],
      activeLow : Boolean,
      cellReset : String)
    case class ClkenInfo(
      modulePort : String,
      activeLow : Boolean,
      cellConst : String)

    case class IrqInfo(
      modulePort : String)

    case class AxiSubInfo(
      modulePort : String,
      clockZynqIdx : Int,
      clockCellReset : String,
      interIdx : Int,
      mapAt : Option[(Long, Option[Long])])
    case class AxiInfo(
      zynqMaster : Boolean,
      zynqPort : String,
      cellConv : String,
      clockZynqIdx : Int,
      clockCellReset : String)

    case class MioInfo(
      modulePort : String,
      isIntf : Boolean,
      zynqPort : String)

    case class ExtInfo(
      modulePort : String,
      isIntf : Boolean,
      extName : String)

    val clockMap   = mutable.Map[String,Int]()
    var clockPorts = Array.fill(4)(None.asInstanceOf[Option[ClockInfo]])
    var resetPorts = Array.fill(4)(None.asInstanceOf[Option[ResetInfo]])
    var clkenPorts = Array.fill(4)(None.asInstanceOf[Option[ClkenInfo]])
    var irqPorts   = Array.fill(16)(None.asInstanceOf[Option[IrqInfo]])

    val axiPorts   = mutable.Map[String, AxiInfo]()
    val axiInter   = mutable.Map[String, mutable.ArrayBuffer[AxiSubInfo]]()
    val axiDirect  = mutable.Map[String, AxiSubInfo]()

    var mioPorts   = mutable.ArrayBuffer[MioInfo]()
    val extPorts   = mutable.ArrayBuffer[ExtInfo]()
    val extMap     = mutable.Map[String,Int]()

    val zynqProps = mutable.Map[String,String]()

    val xdcGen     = new XdcGenerator

    val nameDesign = s"${nameModule}_System"
    val nameWrapper = s"${nameDesign}_wrapper"
    val cellZynqPS = s"${nameModule}_ZynqPS"
    val cellJIrq = s"${nameModule}_IrqJoin"
    val cellNIrq = s"${nameModule}_IrqNull"

    def zynqPropStrings = zynqProps.map(e => s"CONFIG.${e._1} ${e._2}").toSeq

    def setProp(prop : (String, String)) = {
      val exist = zynqProps.get(prop._1)
      if (exist.map(value => value != prop._2).getOrElse(false)) {
        SpinalError("Zynq7: Conflicting config \"${prop._1}\" values \"${prop._2}\" and \"${exist.get}\"")
      } else {
        zynqProps += prop
      }
    }
    def setProps(props : Seq[(String, String)]) = {
      for (prop <- props) {
        setProp(prop)
      }
    }

    def addClock(port : Data, tag : XilinxClock) = {
      val freeIdx = clockPorts.indexWhere(_.isEmpty)
      if (freeIdx < 0) {
        SpinalError("Zynq7 has only 4 clock ports and can not supply ${port}")
      }
      clockMap += (tag.name -> freeIdx)
      clockPorts(freeIdx) = Some(ClockInfo(tag.name))
      setProp(s"PCW_EN_CLK${freeIdx}_PORT" -> "{1}")
      // TODO-lw make configurable?
      // zynqProps += (s"PCW_FCLK${freeIdx}_PERIPHERAL_CLKSRC" -> "{ARM PLL}")
      if (tag.freq.isDefined) {
        val freqMhz = (tag.freq.get / (1 MHz)).toDouble
        setProp(s"PCW_FPGA${freeIdx}_PERIPHERAL_FREQMHZ" -> f"{${freqMhz}%.3f}")
      } else {
        SpinalWarning(s"Zynq7: clock line ${port} has unspecified frequency, using PS default")
      }
    }

    def addReset(port : Data, tag : XilinxReset) = {
      val idx = clockMap.get(tag.clock).getOrElse {
        SpinalError("Zynq7 reset line ${port} associated with unknown clock ${tag.clock}")
      }
      val activeLow = tag.level match {
        case LOW => true
        case _ => false
      }
      resetPorts(idx) = Some(ResetInfo(Some(tag.name), activeLow, s"${nameModule}_Reset_${tag.name}"))
      setProp(s"PCW_EN_RST${idx}_PORT" -> "{1}")
    }

    def addClken(port : Data, tag : XilinxClockEnable) = {
      val idx = clockMap.get(tag.clock).getOrElse {
        SpinalError("Zynq7 clock enable line ${port} associated with unknown clock ${tag.clock}")
      }
      val activeLow = tag.level match {
        case LOW => true
        case _ => false
      }
      clkenPorts(idx) = Some(ClkenInfo(tag.name, activeLow, s"${nameModule}_Reset_${tag.name}"))
    }

    def addIrq(port : Data, tag : XilinxPort, ztag : Zynq7IrqPort) = {
      irqPorts(ztag.idx).map { info =>
        SpinalError(s"Zynq7 interrupt line #${ztag.idx} ${port} already occupied by ${info.modulePort}")
      } getOrElse {
        irqPorts(ztag.idx) = Some(IrqInfo(tag.name))
        setProp(s"PCW_USE_FABRIC_INTERRUPT" -> "{1}")
        setProp(s"PCW_IRQ_F2P_INTR" -> "{1}")
      }
    }

    def addAxi(port : Data, tag : XilinxPort, ztag : Zynq7AxiPort, ctag : ClockDomainTag) = {
      val clockName = ctag.clockDomain.clock.name
      val clockIdx = clockMap.get(clockName).getOrElse {
        SpinalError(s"Zynq7: AXI interface ${port} uses unrecognized clock domain ${clockName}")
      }
      val resetInfo = if (resetPorts(clockIdx).isDefined) {
        resetPorts(clockIdx).get
      } else {
        val resetInfo = ResetInfo(None, false, s"${nameModule}_Reset_${clockIdx}")
        resetPorts(clockIdx) = Some(resetInfo)
        resetInfo
      }

      if (!axiPorts.contains(ztag.name)) {
        val axiInfo = AxiInfo(ztag.isMaster, ztag.name, s"${nameModule}_Inter_${ztag.name}", clockIdx, resetInfo.cellReset)
        axiPorts += (ztag.name -> axiInfo)
        // first reference determines interconnect clock (maybe rather fuzzy, but also matters rarely)
      }
      setProp(s"PCW_USE_${ztag.name}" -> "{1}")
      setProps(ztag.props)

      if (ztag.direct) {
        // request to connect port directly without interconnect
        if (axiInter.contains(ztag.name)) {
          SpinalError(s"Zynq7: Can not connect port ${tag.name} directly to ${ztag.name} already connected to an interconnect")
        }
        if (axiDirect.contains(ztag.name)) {
          SpinalError(s"Zynq7: Can not connect port ${tag.name} directly to ${ztag.name} already connected directly")
        }
        //val axiSubInfo = AxiSubInfo(tag.name, clockIdx, resetInfo.cellReset,  ztag.mapAt)
        axiDirect += (ztag.name -> AxiSubInfo(tag.name, clockIdx, resetInfo.cellReset, 0, ztag.mapAt))
      } else {
        // request to connect via interconnect/converter
        if (axiDirect.contains(ztag.name)) {
          SpinalError(s"Zynq7: Can not connect port ${tag.name} to ${ztag.name} already connected directly")
        }
        val idx = axiInter.getOrElseUpdate(ztag.name, mutable.ArrayBuffer[AxiSubInfo]()).length
        axiInter(ztag.name) += AxiSubInfo(tag.name, clockIdx, resetInfo.cellReset, idx, ztag.mapAt)
      }
    }

    def addMio(port : Data, tag : XilinxPort, ztag : Zynq7MioPort) = {
      mioPorts += MioInfo(tag.name, tag.isInterface, ztag.port)
      setProps(ztag.props)
    }

    def addExt(port : Data, tag : XilinxPort) : Unit = {
      addExt(port, tag.name, tag.isInterface)
    }

    def addExt(port : Data) : Unit = {
      port.flattenForeach { pport =>
        addExt(pport, pport.getName(), false)
      }
    }

    def addExt(port : Data, name : String, isIntf : Boolean) : Unit = {
      val extIdx = extMap.get(name).getOrElse(0)
      extMap += (name -> (extIdx + 1))
      val extName = s"${name}_${extIdx}"
      extPorts += ExtInfo(name, isIntf, s"${name}_${extIdx}")
      xdcGen.analyzePort(port, Some(extName))
    }
  }

  def analyzePorts(pc : PhaseContext) : DesignSpecs = {
    val specs = new DesignSpecs(pc.topLevel.definitionName)

    for (p <- pc.topLevel.getGroupedIO(true)) {
      p.getTag(classOf[XilinxClock]) map { tag =>
        println(s"CLOCK\"${tag.name}\" (RST${tag.reset} CKE${tag.clken} FREQ${tag.freq.map(_.decomposeString)}) = ${p}")
        specs.addClock(p, tag)
      }
    }
    for (p <- pc.topLevel.getGroupedIO(true)) {
      p.getTag(classOf[XilinxReset]) map { tag =>
        println(s"RESET\"${tag.name}\" (CLK\"${tag.clock} ${tag.kind} ${tag.level}) = ${p}")
        specs.addReset(p, tag)
      }
    }
    for (p <- pc.topLevel.getGroupedIO(true)) {
      p.getTag(classOf[XilinxClockEnable]) map { tag =>
        println(s"CLKEN\"${tag.name}\" (CLK\"${tag.clock}\" ${tag.level}) = ${p}")
        specs.addClken(p, tag)
      }
    }
    for (p <- pc.topLevel.getGroupedIO(true)) {
      p.getTag(classOf[XilinxPort]).map { tag =>
        println(s"PORT\"${tag.name}\" ${if(tag.isInterface){"INTF"}else{"PIN"}} CLK\"${tag.clock}\" = ${p}")
        p.getTag(classOf[Zynq7IrqPort]).map { ztag =>
          specs.addIrq(p, tag, ztag)
        } orElse p.getTag(classOf[Zynq7AxiPort]).map { ztag =>
          p.getTag(classOf[ClockDomainTag]) match {
            case Some(ctag) => specs.addAxi(p, tag, ztag, ctag)
            case None => SpinalError(s"Toplevel Axi ${p} must also specify a ClockDomain tag!")
          }
        } orElse p.getTag(classOf[Zynq7MioPort]).map { ztag =>
          specs.addMio(p, tag, ztag)
        } getOrElse {
          SpinalWarning(s"Zynq7: Toplevel ${p} has no Zynq7Port tag, making external")
          specs.addExt(p, tag)
        }
      } orElse p.getTag(classOf[XilinxPlain]).map { tag =>
        println(s"PLAIN ${p}")
        //SpinalWarning(s"Zynq7: Toplevel ${p} has no XilinxPort tag, making external.")
        specs.addExt(p)
      }
    }
    specs
  }

  def generateTcl(ds : DesignSpecs) : String = {
    val buf = new StringBuilder()

    buf ++= s"create_bd_design \"${ds.nameDesign}\"\n\n"
    buf ++= s"create_bd_cell -type ip -vlnv xilinx.com:ip:processing_system7:* ${ds.cellZynqPS}\n"
    buf ++= s"apply_bd_automation -rule xilinx.com:bd_rule:processing_system7 -config {\\\n  make_external \"FIXED_IO, DDR\" \\\n  apply_board_preset \"1\" \\\n  Master \"Disable\" Slave \"Disable\"} \\\n  [get_bd_cells ${ds.cellZynqPS}]\n"
    buf ++= s"set_property -dict [list \\\n  ${ds.zynqPropStrings.mkString("\\\n  ")}] \\\n  [get_bd_cells ${ds.cellZynqPS}]\n\n"
    for (idx <- 0 until 4) {
      ds.resetPorts(idx).map { info =>
        buf ++= s"create_bd_cell -type ip -vlnv xilinx.com:ip:proc_sys_reset:* ${info.cellReset}\n"
        buf ++= s"connect_bd_net [get_bd_pins ${info.cellReset}/slowest_sync_clk] [get_bd_pins ${ds.cellZynqPS}/FCLK_CLK${idx}]\n"
        buf ++= s"connect_bd_net [get_bd_pins ${info.cellReset}/ext_reset_in] [get_bd_pins ${ds.cellZynqPS}/FCLK_RESET${idx}_N]\n\n"
      }
      ds.clkenPorts(idx).map { info =>
        SpinalWarning(s"Zynq7: can not drive toplevel Clock Enable ${ds.nameModule}/{info.modulePort}, constantly active!")
        val constValue = if (info.activeLow) {"0"} else {"1"}
        buf ++= s"create_bd_cell -type ip -vlnv xilinx.com:ip:xlconstant:* ${info.cellConst}\n"
        buf ++= s"set_property -dict [list \\\n CONFIG.CONST_WIDTH {1} \\\n  CONFIG.CONST_VAL {${constValue}}] \\\n  [get_bd_cells ${info.cellConst}]\n"
      }
    }
    if (!ds.irqPorts.forall(_.isEmpty)) {
      buf ++= s"create_bd_cell -type ip -vlnv xilinx.com:ip:xlconcat:* ${ds.cellJIrq}\n"
      buf ++= s"set_property -dict [list \\\n  CONFIG.NUM_PORTS {16}] \\\n  [get_bd_cells ${ds.cellJIrq}]\n"
      buf ++= s"connect_bd_net [get_bd_pins ${ds.cellJIrq}/dout] [get_bd_pins ${ds.cellZynqPS}/IRQ_F2P]\n\n"
      if (!ds.irqPorts.forall(_.isDefined)) {
        buf ++= s"create_bd_cell -type ip -vlnv xilinx.com:ip:xlconstant:* ${ds.cellNIrq}\n"
        buf ++= s"set_property -dict [list \\\n  CONFIG.CONST_WIDTH {1} \\\n  CONFIG.CONST_VAL {0}] \\\n  [get_bd_cells ${ds.cellNIrq}]\n"
        for (idx <- 0 until 16) {
          if (ds.irqPorts(idx).isEmpty) {
            buf ++= s"connect_bd_net [get_bd_pins ${ds.cellJIrq}/In${idx}] [get_bd_pins ${ds.cellNIrq}/dout]\n"
          }
        }
        buf ++= "\n"
      }
    }

    for ((port, info) <- ds.axiPorts) {
      buf ++= s"connect_bd_net [get_bd_pins ${ds.cellZynqPS}/${info.zynqPort}_ACLK] [get_bd_pins ${ds.cellZynqPS}/FCLK_CLK${info.clockZynqIdx}]\n"
      ds.axiInter.get(port).map { subInfos =>
        val numSI = if (info.zynqMaster) {1} else {subInfos.length}
        val numMI = if (info.zynqMaster) {subInfos.length} else {1}
        buf ++= s"create_bd_cell -type ip -vlnv xilinx.com:ip:axi_interconnect:* ${info.cellConv}\n"
        buf ++= s"set_property -dict [list \\\n"
        for (idx <- 0 until numSI) {
          buf ++= f"  CONFIG.S${idx}%02d_HAS_REGSLICE {4} \\\n"
        }
        for (idx <- 0 until numMI) {
          buf ++= f"  CONFIG.M${idx}%02d_HAS_REGSLICE {4} \\\n"
        }
        buf ++= s"  CONFIG.NUM_SI {${numSI}} \\\n"
        buf ++= s"  CONFIG.NUM_MI {${numMI}} ] \\\n"
        buf ++= s"  [get_bd_cells ${info.cellConv}]\n"
        buf ++= s"connect_bd_net [get_bd_pins ${info.cellConv}/ACLK] [get_bd_pins ${ds.cellZynqPS}/FCLK_CLK${info.clockZynqIdx}]\n"
        buf ++= s"connect_bd_net [get_bd_pins ${info.cellConv}/ARESETN] [get_bd_pins ${info.clockCellReset}/interconnect_aresetn]\n"
        val zynqSide = (if (info.zynqMaster) {"S"} else {"M"})
        val designSide = (if (info.zynqMaster) {"M"} else {"S"})
        buf ++= s"connect_bd_net [get_bd_pins ${info.cellConv}/${zynqSide}00_ACLK] [get_bd_pins ${ds.cellZynqPS}/FCLK_CLK${info.clockZynqIdx}]\n"
        buf ++= s"connect_bd_net [get_bd_pins ${info.cellConv}/${zynqSide}00_ARESETN] [get_bd_pins ${info.clockCellReset}/interconnect_aresetn]\n"
        for (subInfo <- subInfos) {
          buf ++= f"connect_bd_net [get_bd_pins ${info.cellConv}/${designSide}${subInfo.interIdx}%02d_ACLK] [get_bd_pins ${ds.cellZynqPS}/FCLK_CLK${subInfo.clockZynqIdx}]\n"
          buf ++= f"connect_bd_net [get_bd_pins ${info.cellConv}/${designSide}${subInfo.interIdx}%02d_ARESETN] [get_bd_pins ${subInfo.clockCellReset}/interconnect_aresetn]\n"
        }
        buf ++= s"connect_bd_intf_net [get_bd_intf_pins ${info.cellConv}/${zynqSide}00_AXI] [get_bd_intf_pins ${ds.cellZynqPS}/${info.zynqPort}]\n"
      }
    }

    buf ++= "\n"
    buf ++= s"create_bd_cell -type module -reference ${ds.nameModule} ${ds.nameModule}\n"
    for (idx <- 0 until 4) {
      ds.clockPorts(idx).map { info =>
        buf ++= s"connect_bd_net [get_bd_pins ${ds.nameModule}/${info.modulePort}] [get_bd_pins ${ds.cellZynqPS}/FCLK_CLK${idx}]\n"
      }
      ds.resetPorts(idx).map { info =>
        if (info.modulePort.isDefined) {
          val resetPort = if (info.activeLow) {"peripheral_aresetn"} else {"peripheral_reset"}
          buf ++= s"connect_bd_net [get_bd_pins ${ds.nameModule}/${info.modulePort.get}] [get_bd_pins ${info.cellReset}/${resetPort}]\n"
        }
      }
      ds.clkenPorts(idx).map { info =>
        buf ++= s"connect_bd_net [get_bd_pins ${ds.nameModule}/${info.modulePort}] [get_bd_pins ${info.cellConst}/dout]\n"
      }
    }
    for (idx <- 0 until 16) {
      ds.irqPorts(idx).map { info =>
        buf ++= s"connect_bd_net [get_bd_pins ${ds.nameModule}/${info.modulePort}] [get_bd_pins ${ds.cellJIrq}/In${idx}]\n"
      }
    }

    for ((port, subInfo) <- ds.axiDirect) {
      ds.axiPorts.get(port).map { info =>
        buf ++= s"connect_bd_intf_net [get_bd_intf_pins ${ds.nameModule}/${subInfo.modulePort}] [get_bd_intf_pins ${ds.cellZynqPS}/${info.zynqPort}]\n"
      }
    }
    for ((port, subInfos) <- ds.axiInter) {
      ds.axiPorts.get(port).map { info =>
        for (subInfo <- subInfos) {
          val convPort = f"${if(info.zynqMaster){"M"}else{"S"}}${subInfo.interIdx}%02d_AXI"
          buf ++= s"connect_bd_intf_net [get_bd_intf_pins ${ds.nameModule}/${subInfo.modulePort}] [get_bd_intf_pins ${info.cellConv}/${convPort}]\n"
        }
      }
    }
    for (info <- ds.mioPorts) {
      if (info.isIntf) {
        buf ++= s"connect_bd_intf_net [get_bd_intf_pins ${ds.nameModule}/${info.modulePort}] [get_bd_intf_pins ${ds.cellZynqPS}/${info.zynqPort}]\n"
      } else {
        buf ++= s"connect_bd_net [get_bd_pins ${ds.nameModule}/${info.modulePort}] [get_bd_pins ${ds.cellZynqPS}/${info.zynqPort}]\n"
      }
    }
    for (info <- ds.extPorts) {
      if (info.isIntf) {
        buf ++= s"make_bd_intf_pins_external [get_bd_intf_pins ${ds.nameModule}/${info.modulePort}]\n"
      } else {
        buf ++= s"make_bd_pins_external [get_bd_pins ${ds.nameModule}/${info.modulePort}]\n"
      }
    }

    buf ++= "\n"
    for ((port, subInfos) <- ds.axiInter) {
      ds.axiPorts.get(port).map { info =>
        for (subInfo <- subInfos) {
          subInfo.mapAt.map { spec =>
            val rangeOption = spec._2.map(size => f" -range 0x${size}%x").getOrElse("")
            buf ++= f"assign_bd_address -target_address_space /${ds.cellZynqPS}/Data [get_bd_addr_segs ${ds.nameModule}/${subInfo.modulePort}/reg0] -offset 0x${spec._1}%x${rangeOption}\n"
          }
        }
      }
    }
    buf ++= "assign_bd_address\n"

    buf ++= "\n"
    buf ++= "validate_bd_design\n"
    buf ++= "save_bd_design [current_bd_design]\n"
    buf ++= "close_bd_design [current_bd_design]\n\n"

    buf ++= s"make_wrapper -top -import [get_files ${ds.nameDesign}.bd]\n"

    buf.toString
  }

  override def impl(pc : PhaseContext) = {
    val specs = analyzePorts(pc)
    val tcl = generateTcl(specs)
    pc.topLevel.addTag(ProjectArtifact(s"${specs.nameWrapper}.xdc", specs.xdcGen.getXdc(), ProjectConstr))
    pc.topLevel.addTag(ProjectScript(tcl, "Create System Wrapper BlockDesign"))
    pc.topLevel.addTag(ProjectToplevel(specs.nameWrapper))
  }

  override def hasNetlistImpact = false
}

