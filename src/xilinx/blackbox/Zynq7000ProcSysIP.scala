package hwlib.xilinx.blackbox

import scala.collection.mutable

import spinal.core._
import spinal.lib.{master, slave}

import hwlib.xilinx.tools.ProjectScript
import hwlib.amba.{TAxi, AxiConfig}
import hwlib.serial.{TSerial, SerialConfig}



case class Zynq7000ProcSysConfig(
  IpConfig : Map[String, String] = Map(),
  IpVendor : String = "xilinx.com",
  IpLibrary : String = "ip",
  IpName : String = "processing_system7",
  IpVersion : String = "5.5") {

  def getConfig(key : String, default : String = "") = IpConfig.getOrElse(key, default)
  def getConfigInt(key : String, default : Int = 0) = IpConfig.get(key).map(_.toInt).getOrElse(default)
  def getConfigBool(key : String, default : Boolean = false) = IpConfig.get(key).map(_ == "1").getOrElse(default)
}

class TZynq7000Fixed extends Bundle {
    val MIO          = Bits(54 bits)
    val DDR_Clk      = Bool
    val DDR_Clk_n    = Bool
    val DDR_CKE      = Bool
    val DDR_DRSTB    = Bool
    val DDR_CS_n     = Bool
    val DDR_RAS_n    = Bool
    val DDR_CAS_n    = Bool
    val DDR_WEB      = Bool
    val DDR_BankAddr = Bits( 3 bits)
    val DDR_Addr     = Bits(15 bits)
    val DDR_DM       = Bits( 4 bits)
    val DDR_DQ       = Bits(32 bits)
    val DDR_DQS_n    = Bits( 4 bits)
    val DDR_DQS      = Bits( 4 bits)
    val DDR_ODT      = Bool
    val DDR_VRN      = Bool
    val DDR_VRP      = Bool
    val PS_SRSTB     = Bool
    val PS_CLK       = Bool
    val PS_PORB      = Bool
}


class Zynq7000ProcSysIP(val cfg : Zynq7000ProcSysConfig) extends BlackBox {

  val BaseAxiConfig= AxiConfig(
    addrBits = 32,
    dataBytes = 4,
    idBits = 6,
    hasStrb = true,
    hasBurst = true,
    hasLock = true,
    hasCache = true,
    hasProt = true,
    hasQos = true,
    isAxi3 = true)

  val hasMGP0 = cfg.getConfigBool("CONFIG.PCW_USE_M_AXI_GP0", default=true)
  val isMGP0Remap = cfg.getConfigBool("CONFIG.PCW_M_AXI_GP0_ENABLE_STATIC_REMAP")

  val hasMGP1 = cfg.getConfigBool("CONFIG.PCW_USE_M_AXI_GP1")
  val isMGP1Remap = cfg.getConfigBool("CONFIG.PCW_M_AXI_GP1_ENABLE_STATIC_REMAP")

  val hasSGP0 = cfg.getConfigBool("CONFIG.PCW_USE_S_AXI_GP0")

  val hasSGP1 = cfg.getConfigBool("CONFIG.PCW_USE_S_AXI_GP1")

  val hasSACP = cfg.getConfigBool("CONFIG.PCW_USE_S_AXI_ACP")

  val hasSHP0 = cfg.getConfigBool("CONFIG.PCW_USE_S_AXI_HP0")
  val widthSHP0 = cfg.getConfigInt("CONFIG.PCW_S_AXI_HP0_DATA_WIDTH", 64)

  val hasSHP1 = cfg.getConfigBool("CONFIG.PCW_USE_S_AXI_HP1")
  val widthSHP1 = cfg.getConfigInt("CONFIG.PCW_S_AXI_HP1_DATA_WIDTH", 64)

  val hasSHP2 = cfg.getConfigBool("CONFIG.PCW_USE_S_AXI_HP2")
  val widthSHP2 = cfg.getConfigInt("CONFIG.PCW_S_AXI_HP2_DATA_WIDTH", 64)

  val hasSHP3 = cfg.getConfigBool("CONFIG.PCW_USE_S_AXI_HP3")
  val widthSHP3 = cfg.getConfigInt("CONFIG.PCW_S_AXI_HP3_DATA_WIDTH", 64)

  val hasClk0 = cfg.getConfigBool("CONFIG.PCW_EN_CLK0_PORT", true)
  val hasClk1 = cfg.getConfigBool("CONFIG.PCW_EN_CLK1_PORT")
  val hasClk2 = cfg.getConfigBool("CONFIG.PCW_EN_CLK2_PORT")
  val hasClk3 = cfg.getConfigBool("CONFIG.PCW_EN_CLK3_PORT")
  val hasRst0 = cfg.getConfigBool("CONFIG.PCW_EN_RST0_PORT", true)
  val hasRst1 = cfg.getConfigBool("CONFIG.PCW_EN_RST1_PORT")
  val hasRst2 = cfg.getConfigBool("CONFIG.PCW_EN_RST2_PORT")
  val hasRst3 = cfg.getConfigBool("CONFIG.PCW_EN_RST3_PORT")

  val hasIrq = cfg.getConfigBool("CONFIG.PCW_USE_FABRIC_INTERRUPT") &&
               cfg.getConfigBool("CONFIG.PCW_IRQ_F2P_INTR")
  val widthIrq = cfg.getConfigInt("CONFIG.PCW_NUM_F2P_INTR_INPUTS", 1)

  val cfgMGP0 = Option.when(hasMGP0)(BaseAxiConfig.copy(idBits = if(isMGP0Remap) { 6 } else { 12 }))
  val cfgMGP1 = Option.when(hasMGP1)(BaseAxiConfig.copy(idBits = if(isMGP1Remap) { 6 } else { 12 }))

  val cfgSGP0 = Option.when(hasSGP0)(BaseAxiConfig)
  val cfgSGP1 = Option.when(hasSGP1)(BaseAxiConfig)
  val cfgSACP = Option.when(hasSACP)(BaseAxiConfig.copy(dataBytes = 8, idBits = 3, aUserBits = 5))
  val cfgSHP0 = Option.when(hasSHP0)(BaseAxiConfig.copy(dataBytes = widthSHP0 / 8))
  val cfgSHP1 = Option.when(hasSHP1)(BaseAxiConfig.copy(dataBytes = widthSHP1 / 8))
  val cfgSHP2 = Option.when(hasSHP2)(BaseAxiConfig.copy(dataBytes = widthSHP2 / 8))
  val cfgSHP3 = Option.when(hasSHP3)(BaseAxiConfig.copy(dataBytes = widthSHP3 / 8))

  val hasUart0 = cfg.getConfigBool("CONFIG.PCW_UART0_PERIPHERAL_ENABLE")
  val dataUart0 = cfg.getConfig("CONFIG.PCW_UART0_UART0_IO") == "EMIO"
  val modemUart0 = cfg.getConfigBool("CONFIG.PCW_UART0_GRP_FULL_ENABLE")

  val cfgUart0 = Option.when(hasUart0 && (dataUart0 || modemUart0))(SerialConfig(
        hasData=dataUart0,
        hasHandshake=modemUart0, negHandshake=true,
        hasPresence=modemUart0, negPresence=true,
        hasModem=modemUart0, negCarrier=true, negRing=true))

  val io = new Bundle {
    val fixed = inout(Analog(new TZynq7000Fixed))

    val fclk_clk0 = ifGen(hasClk0) { out(Bool) }
    val fclk_clk1 = ifGen(hasClk1) { out(Bool) }
    val fclk_clk2 = ifGen(hasClk2) { out(Bool) }
    val fclk_clk3 = ifGen(hasClk3) { out(Bool) }
    val fclk_reset0_n = ifGen(hasRst0) { out(Bool) }
    val fclk_reset1_n = ifGen(hasRst1) { out(Bool) }
    val fclk_reset2_n = ifGen(hasRst2) { out(Bool) }
    val fclk_reset3_n = ifGen(hasRst3) { out(Bool) }

    val irq_f2p = ifGen(hasIrq) { in(Bits(widthIrq bits)) }

    val m_axi_gp0_aclk = ifGen(cfgMGP0.isDefined) { in(Bool) }
    val m_axi_gp0 = ifGen(cfgMGP0.isDefined) { master(TAxi(cfgMGP0.get)) }

    val m_axi_gp1_aclk = ifGen(cfgMGP1.isDefined) { in(Bool) }
    val m_axi_gp1 = ifGen(cfgMGP1.isDefined) { master(TAxi(cfgMGP1.get)) }

    val s_axi_gp0_aclk = ifGen(cfgSGP0.isDefined) { in(Bool) }
    val s_axi_gp0 = ifGen(cfgSGP0.isDefined) { slave(TAxi(cfgSGP0.get)) }

    val s_axi_gp1_aclk = ifGen(cfgSGP1.isDefined) { in(Bool) }
    val s_axi_gp1 = ifGen(cfgSGP1.isDefined) { slave(TAxi(cfgSGP1.get)) }

    val s_axi_acp_aclk = ifGen(cfgSACP.isDefined) { in(Bool) }
    val s_axi_acp = ifGen(cfgSACP.isDefined) { slave(TAxi(cfgSACP.get)) }

    val s_axi_hp0_aclk = ifGen(cfgSHP0.isDefined) { in(Bool) }
    val s_axi_hp0 = ifGen(cfgSHP0.isDefined) { slave(TAxi(cfgSHP0.get)) }

    val s_axi_hp1_aclk = ifGen(cfgSHP1.isDefined) { in(Bool) }
    val s_axi_hp1 = ifGen(cfgSHP1.isDefined) { slave(TAxi(cfgSHP1.get)) }

    val s_axi_hp2_aclk = ifGen(cfgSHP2.isDefined) { in(Bool) }
    val s_axi_hp2 = ifGen(cfgSHP2.isDefined) { slave(TAxi(cfgSHP2.get)) }

    val s_axi_hp3_aclk = ifGen(cfgSHP3.isDefined) { in(Bool) }
    val s_axi_hp3 = ifGen(cfgSHP3.isDefined) { slave(TAxi(cfgSHP3.get)) }

    val uart0 = ifGen(cfgUart0.isDefined) { master(TSerial(cfgUart0.get)) }
  }

  io.fixed.setPartialName("")
  if (cfgMGP0.isDefined) { io.m_axi_gp0.nameThis() }
  if (cfgMGP1.isDefined) { io.m_axi_gp1.nameThis() }
  if (cfgSGP0.isDefined) { io.s_axi_gp0.nameThis() }
  if (cfgSGP1.isDefined) { io.s_axi_gp1.nameThis() }
  if (cfgSACP.isDefined) { io.s_axi_acp.nameThis() }
  if (cfgSHP0.isDefined) { io.s_axi_hp0.nameThis() }
  if (cfgSHP1.isDefined) { io.s_axi_hp1.nameThis() }
  if (cfgSHP2.isDefined) { io.s_axi_hp2.nameThis() }
  if (cfgSHP3.isDefined) { io.s_axi_hp3.nameThis() }

  def getCreateScript = {
    val buf = new StringBuilder()
    buf ++= s"create_ip -vendor ${cfg.IpVendor} -library ${cfg.IpLibrary} -name ${cfg.IpName} -version ${cfg.IpVersion} -module_name {{compName}}\n"
    buf ++= s"set_property -dict {\n    "
    buf ++= cfg.IpConfig.map(e => s"${e._1} {${e._2}}").mkString("\n    ")
    buf ++= s"}  [get_ips {{compName}}]\n"
    buf ++= s"generate_target all [get_ips {{compName}}] -force\n"

    ProjectScript(buf.toString)
  }

  noIoPrefix()
  addTag(noNumericType)
  addTag(getCreateScript)
}

