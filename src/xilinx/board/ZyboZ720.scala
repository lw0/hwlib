package hwlib.xilinx.board

import spinal.core._

import hwlib.xilinx.tools.{XdcInfo, Pin, Pins, Std, Stds, Pull, Clock, Rename}


object ZyboZ720 extends Board {
  def part = "xc7z020clg400-1"
  def board = Some("digilentinc.com:zybo-z7-20:part0:1.2")
  def ports = Map(
    "FIXED" -> XdcInfo.multi(
      "MIO" ->          Seq(Rename("MIO")),
      "DDR_Clk" ->      Seq(Rename("DDR_Clk")),
      "DDR_Clk_n" ->    Seq(Rename("DDR_Clk_n")),
      "DDR_CKE" ->      Seq(Rename("DDR_CKE")),
      "DDR_DRSTB" ->    Seq(Rename("DDR_DRSTB")),
      "DDR_CS_n" ->     Seq(Rename("DDR_CS_n")),
      "DDR_RAS_n" ->    Seq(Rename("DDR_RAS_n")),
      "DDR_CAS_n" ->    Seq(Rename("DDR_CAS_n")),
      "DDR_WEB" ->      Seq(Rename("DDR_WEB")),
      "DDR_BankAddr" -> Seq(Rename("DDR_BankAddr")),
      "DDR_Addr" ->     Seq(Rename("DDR_Addr")),
      "DDR_DM" ->       Seq(Rename("DDR_DM")),
      "DDR_DQ" ->       Seq(Rename("DDR_DQ")),
      "DDR_DQS_n" ->    Seq(Rename("DDR_DQS_n")),
      "DDR_DQS" ->      Seq(Rename("DDR_DQS")),
      "DDR_ODT" ->      Seq(Rename("DDR_ODT")),
      "DDR_VRN" ->      Seq(Rename("DDR_VRN")),
      "DDR_VRP" ->      Seq(Rename("DDR_VRP")),
      "PS_SRSTB" ->     Seq(Rename("PS_SRSTB")),
      "PS_CLK" ->       Seq(Rename("PS_CLK")),
      "PS_PORB" ->      Seq(Rename("PS_PORB"))),

    "CLK100M" -> XdcInfo(
      // Sch=sysclk
      Pin("K17"), Std("LVCMOS33"), Clock(8.0)),

    "SWITCH" -> XdcInfo(
      // Sch=SW{0..3}
      Pins("G15", "P15", "W13", "T16"),
       Std("LVCMOS33")),

    "BUTTON" -> XdcInfo(
      // Sch=BTN{0..3}
      Pins("K18", "P16", "K19", "Y16"),
       Std("LVCMOS33")),

    "LED" -> XdcInfo(
      // Sch=LED{0..3}
      Pins("M14", "M15", "G14", "D18"),
       Std("LVCMOS33")),

    "RLED" -> XdcInfo.multi(
      "red" -> Seq(
        // Sch=LED5_R
        Pin("Y11"), Std("LVCMOS33")),
      "green" -> Seq(
        // Sch=LED5_G
        Pin("T5"), Std("LVCMOS33")),
      "blue" -> Seq(
        // Sch=LED5_B
        Pin("Y12"), Std("LVCMOS33"))),

    "LLED" -> XdcInfo.multi(
      "red" -> Seq(
        // Sch=LED6_R
        Pin("V16"), Std("LVCMOS33")),
      "green" -> Seq(
        // Sch=LED6_G
        Pin("F17"), Std("LVCMOS33")),
      "blue" -> Seq(
        // Sch=LED6_B
        Pin("M17"), Std("LVCMOS33"))),

    "AUDIO" -> XdcInfo.multi(
      "bclk" -> Seq(
        // Sch=AC_BCLK
        Pin("R19"), Std("LVCMOS33")),
      "mclk" -> Seq(
        // Sch=AC_MCLK
        Pin("R17"), Std("LVCMOS33")),
      "muten" -> Seq(
        // Sch=AC_MUTEN
        Pin("P18"), Std("LVCMOS33")),
      "plysdata" -> Seq(
        // Sch=AC_PBDAT
        Pin("R18"), Std("LVCMOS33")),
      "plylrclk" -> Seq(
        // Sch=AC_PBLRC
        Pin("T19"), Std("LVCMOS33")),
      "recsdata" -> Seq(
        // Sch=AC_RECDAT
        Pin("R16"), Std("LVCMOS33")),
      "reclrclk" -> Seq(
        // Sch=AC_RECLRC
        Pin("Y18"), Std("LVCMOS33")),
      "scl" -> Seq(
        // Sch=AC_SCL
        Pin("N18"), Std("LVCMOS33")),
      "sda" -> Seq(
        // Sch=AC_SDA
        Pin("N17"), Std("LVCMOS33"))),

    "ETHAUX" -> XdcInfo.multi(
      "clk" -> Seq(
        // Sch=SYSCLK
        Pin("K17"), Std("LVCMOS33"), Clock(8.0)),
      "intb" -> Seq(
        // Sch=ETH_INT_PU_B
        Pin("F16"), Std("LVCMOS33"), Pull("PULLUP")),
      "rstb" -> Seq(
        // Sch=ETH_RST_B
        Pin("E17"), Std("LVCMOS33"))),

    "USB_OVERCURR" -> XdcInfo(
      // Sch=OTG_OC
      Pin("U13"), Std("LVCMOS33")),

    "FAN_FEEDBACK" -> XdcInfo(
      // Sch=FAN_FB_PU
      Pin("Y13"), Std("LVCMOS33"), Pull("PULLUP")),

    "HDMI_SINK" -> XdcInfo.multi(
      "hpa" -> Seq(
        // Sch=hdmi_rx_hpd
        Pin("W19"), Std("LVCMOS33")),
      "scl" -> Seq(
        // Sch=hdmi_rx_scl
        Pin("W18"), Std("LVCMOS33")),
      "sda" -> Seq(
        // Sch=hdmi_rx_sda
        Pin("Y19"), Std("LVCMOS33")),
      "cec" -> Seq(
        // Sch=hdmi_rx_cec
        Pin("Y8"), Std("LVCMOS33")),
      "clk_n" -> Seq(
        // Sch=hdmi_rx_clk_n
        Pin("U19"), Std("TMDS_33")),
      "clk_p" -> Seq(
        // Sch=hdmi_rx_clk_p
        Pin("U18"), Std("TMDS_33")),
      "dat_n" -> Seq(
        // Sch=hdmi_rx_n{0..2}
        Pins("W20", "U20", "P20"),
         Std("TMDS_33")),
      "dat_p" -> Seq(
        // Sch=hdmi_rx_p{0..2}
        Pins("V20", "T20", "N20"),
         Std("TMDS_33"))),

    "HDMI_SOURCE" -> XdcInfo.multi(
      "hpd" -> Seq(
        // Sch=hdmi_tx_hpd
        Pin("E18"), Std("LVCMOS33")),
      "scl" -> Seq(
        // Sch=hdmi_tx_scl
        Pin("G17"), Std("LVCMOS33")),
      "sda" -> Seq(
        // Sch=hdmi_tx_sda
        Pin("G18"), Std("LVCMOS33")),
      "cec" -> Seq(
        // Sch=hdmi_tx_cec
        Pin("E19"), Std("LVCMOS33")),
      "clk_n" -> Seq(
        // Sch=hdmi_tx_clk_n
        Pin("H17"), Std("TMDS_33")),
      "clk_p" -> Seq(
        // Sch=hdmi_tx_clk_p
        Pin("H16"), Std("TMDS_33")),
      "dat_n" -> Seq(
        // Sch=hdmi_tx_n{0..2}
        Pins("D20", "B20", "A20"),
         Std("TMDS_33")),
      "dat_p" -> Seq(
        // Sch=hdmi_tx_p{0..2}
        Pins("D19", "C20", "B19"),
         Std("TMDS_33"))),

    "PMODA" -> XdcInfo.multi(
      "n" -> Seq(
        // Sch=JA{1..4}_R_N
        Pins("N16", "L15", "J16", "J14"),
         Std("LVCMOS33")),
      "p" -> Seq(
        // Sch=JA{1..4}_R_P
        Pins("N15", "L14", "K16", "K14"),
         Std("LVCMOS33"))),

    "PMODB" -> XdcInfo.multi(
      "n" -> Seq(
        // Sch=JB{1..4}_N
        Pins("W8", "V7", "Y6", "W6"),
         Std("LVCMOS33")),
      "p" -> Seq(
        // Sch=JB{1..4}_P
        Pins("V8", "U7", "Y7", "V6"),
         Std("LVCMOS33"))),

    "PMODC" -> XdcInfo.multi(
      "n" -> Seq(
        // Sch=JC{1..4}_N
        Pins("W15", "T10", "Y14", "U12"),
         Std("LVCMOS33")),
      "p" -> Seq(
        // Sch=JC{1..4}_P
        Pins("V15", "T11", "W14", "T12"),
         Std("LVCMOS33"))),

    "PMODD" -> XdcInfo.multi(
      "n" -> Seq(
        // Sch=JD{1..4}_N
        Pins("T14", "P14", "U14", "V17"),
         Std("LVCMOS33")),
      "p" -> Seq(
        // Sch=JD{1..4}_P
        Pins("T15", "R14", "U15", "V18"),
         Std("LVCMOS33"))),

    "PMODE" -> XdcInfo(
      // Sch=JE{1..4,7..10}
      Pins("V12", "W16", "J15", "H15", "V13", "U17", "T17", "Y17"),
       Std("LVCMOS33")),

    "PCAM" -> XdcInfo.multi(
      //# set_property INTERNAL_VREF 0.6 [get_iobanks 35]
      "clk_lp_n" -> Seq(
        // Sch=lp_clk_n
        Pin("J19"), Std("HSUL_12")),
      "clk_lp_p" -> Seq(
        // Sch=lp_clk_p
        Pin("H20"), Std("HSUL_12")),
      "data_lp_n" -> Seq(
        // Sch=lp_lane_n{0..1}
        Pins("M18", "L20"), Std("HSUL_12")),
      "data_lp_p" -> Seq(
        // Sch=lp_lane_p{0..1}
        Pins("L19", "J20"), Std("HSUL_12")),
      "clk_hs_n" -> Seq(
        // Sch=mipi_clk_n
        Pin("H18"), Std("LVDS_25")),
      "clk_hs_p" -> Seq(
        // Sch=mipi_clk_p
        Pin("J18"), Std("LVDS_25"), Clock(2.976)),
      "data_hs_n" -> Seq(
        // Sch=mipi_lane_n{0..1}
        Pins("M20", "L17"), Std("LVDS_25")),
      "data_hs_p" -> Seq(
        // Sch=mipi_lane_p{0..1}
        Pins("M19", "L16"), Std("LVDS_25")),
      "clk" -> Seq(
        // Sch=cam_clk
        Pin("G19"), Std("LVCMOS33")),
      "gpio" -> Seq(
        // Sch=cam_gpio
        Pin("G20"), Std("LVCMOS33"), Pull("PULLUP")),
      "scl" -> Seq(
        // Sch=cam_scl
        Pin("F20"), Std("LVCMOS33")),
      "sda" -> Seq(
        // Sch=cam_sda
        Pin("F19"), Std("LVCMOS33"))))
  val zynqBaseCfg = Map(
    "CONFIG.PCW_FCLK0_PERIPHERAL_CLKSRC" -> "ARM PLL",
    "CONFIG.PCW_FPGA0_PERIPHERAL_FREQMHZ" -> "111.111",
    "CONFIG.PCW_EN_CLK0_PORT" -> "1",
    "CONFIG.PCW_EN_CLK1_PORT" -> "0",
    "CONFIG.PCW_EN_CLK2_PORT" -> "0",
    "CONFIG.PCW_EN_CLK3_PORT" -> "0",
    "CONFIG.PCW_EN_RST0_PORT" -> "1",
    "CONFIG.PCW_EN_RST1_PORT" -> "0",
    "CONFIG.PCW_EN_RST2_PORT" -> "0",
    "CONFIG.PCW_EN_RST3_PORT" -> "0",
    "CONFIG.PCW_USE_FABRIC_INTERRUPT" -> "0",

    "CONFIG.PCW_USE_M_AXI_GP0" -> "0",
    "CONFIG.PCW_USE_M_AXI_GP1" -> "0",
    "CONFIG.PCW_USE_S_AXI_GP0" -> "0",
    "CONFIG.PCW_USE_S_AXI_GP1" -> "0",
    "CONFIG.PCW_USE_S_AXI_ACP" -> "0",
    "CONFIG.PCW_USE_S_AXI_HP0" -> "0",
    "CONFIG.PCW_USE_S_AXI_HP1" -> "0",
    "CONFIG.PCW_USE_S_AXI_HP2" -> "0",
    "CONFIG.PCW_USE_S_AXI_HP3" -> "0",

    "CONFIG.PCW_PRESET_BANK0_VOLTAGE"                  -> "LVCMOS 3.3V",
    "CONFIG.PCW_PRESET_BANK1_VOLTAGE"                  -> "LVCMOS 1.8V",

    "CONFIG.PCW_USB_RESET_ENABLE"                      -> "1",
    "CONFIG.PCW_USB_RESET_POLARITY"                    -> "Active Low",
    "CONFIG.PCW_USB_RESET_SELECT"                      -> "Share reset pin",

    "CONFIG.PCW_USB0_PERIPHERAL_ENABLE"                -> "1",
    "CONFIG.PCW_USB0_USB0_IO"                          -> "MIO 28 .. 39",
    "CONFIG.PCW_USB0_RESET_ENABLE"                     -> "1",
    "CONFIG.PCW_USB0_RESET_IO"                         -> "MIO 46",

    "CONFIG.PCW_ENET_RESET_ENABLE"                     ->"1",
    "CONFIG.PCW_ENET_RESET_POLARITY"                   ->"Active Low",
    "CONFIG.PCW_ENET_RESET_SELECT"                     ->"Share reset pin",

    "CONFIG.PCW_ENET0_PERIPHERAL_ENABLE"               ->"1",
    "CONFIG.PCW_ENET0_ENET0_IO"                        ->"MIO 16 .. 27",
    "CONFIG.PCW_ENET0_GRP_MDIO_ENABLE"                 ->"1",
    "CONFIG.PCW_ENET0_GRP_MDIO_IO"                     ->"MIO 52 .. 53",

    "CONFIG.PCW_ENET0_PERIPHERAL_CLKSRC"               ->"IO PLL",
    "CONFIG.PCW_ENET0_PERIPHERAL_DIVISOR0"             ->"8",
    "CONFIG.PCW_ENET0_PERIPHERAL_DIVISOR1"             ->"1",
    "CONFIG.PCW_ENET0_PERIPHERAL_FREQMHZ"              ->"1000 Mbps",

    "CONFIG.PCW_QSPI_GRP_FBCLK_ENABLE"                 ->"1",
    "CONFIG.PCW_QSPI_GRP_FBCLK_IO"                     ->"MIO 8",
    "CONFIG.PCW_QSPI_GRP_IO1_ENABLE"                   ->"0",
    "CONFIG.PCW_QSPI_GRP_SINGLE_SS_ENABLE"             ->"1",
    "CONFIG.PCW_QSPI_GRP_SINGLE_SS_IO"                 ->"MIO 1 .. 6",
    "CONFIG.PCW_QSPI_GRP_SS1_ENABLE"                   ->"0",
    "CONFIG.PCW_QSPI_QSPI_IO"                          ->"MIO 1 .. 6",

    "CONFIG.PCW_SD0_PERIPHERAL_ENABLE"                 -> "1",
    "CONFIG.PCW_SD0_SD0_IO"                            -> "MIO 40 .. 45",
    "CONFIG.PCW_SD0_GRP_CD_ENABLE"                     -> "1",
    "CONFIG.PCW_SD0_GRP_CD_IO"                         -> "MIO 47",
    "CONFIG.PCW_SD0_GRP_POW_ENABLE"                    ->"0",
    "CONFIG.PCW_SD0_GRP_WP_ENABLE"                     ->"0",

    "CONFIG.PCW_UART1_PERIPHERAL_ENABLE"               ->"1",
    "CONFIG.PCW_UART1_UART1_IO"                        ->"MIO 48 .. 49",
    "CONFIG.PCW_UART1_GRP_FULL_ENABLE"                 ->"0",
    "CONFIG.PCW_UART1_BAUD_RATE"                       ->"115200",

    "CONFIG.PCW_GPIO_MIO_GPIO_ENABLE"                  ->"1",
    "CONFIG.PCW_GPIO_MIO_GPIO_IO"                      ->"MIO",
    "CONFIG.PCW_GPIO_PERIPHERAL_ENABLE"                ->"0",

    "CONFIG.PCW_UIPARAM_DDR_ENABLE"                    ->"1",
    "CONFIG.PCW_UIPARAM_DDR_DRAM_WIDTH"                ->"16 Bits",
    "CONFIG.PCW_UIPARAM_DDR_ECC"                       ->"Disabled",
    "CONFIG.PCW_UIPARAM_DDR_FREQ_MHZ"                  ->"533.333333",
    "CONFIG.PCW_UIPARAM_DDR_HIGH_TEMP"                 ->"Normal (0-85)",
    "CONFIG.PCW_UIPARAM_DDR_MEMORY_TYPE"               ->"DDR 3 (Low Voltage)",
    "CONFIG.PCW_UIPARAM_DDR_PARTNO"                    ->"MT41K256M16 RE-125",
    "CONFIG.PCW_UIPARAM_DDR_SPEED_BIN"                 ->"DDR3_1066F",
    "CONFIG.PCW_UIPARAM_DDR_TRAIN_DATA_EYE"            ->"1",
    "CONFIG.PCW_UIPARAM_DDR_TRAIN_READ_GATE"           ->"1",
    "CONFIG.PCW_UIPARAM_DDR_TRAIN_WRITE_LEVEL"         ->"1",
    "CONFIG.PCW_UIPARAM_DDR_USE_INTERNAL_VREF"         ->"0",

    "CONFIG.PCW_UIPARAM_DDR_BOARD_DELAY0"              ->"0.221",
    "CONFIG.PCW_UIPARAM_DDR_BOARD_DELAY1"              ->"0.222",
    "CONFIG.PCW_UIPARAM_DDR_BOARD_DELAY2"              ->"0.217",
    "CONFIG.PCW_UIPARAM_DDR_BOARD_DELAY3"              ->"0.244",
    "CONFIG.PCW_UIPARAM_DDR_DQS_TO_CLK_DELAY_0"        ->"-0.050",
    "CONFIG.PCW_UIPARAM_DDR_DQS_TO_CLK_DELAY_1"        ->"-0.044",
    "CONFIG.PCW_UIPARAM_DDR_DQS_TO_CLK_DELAY_2"        ->"-0.035",
    "CONFIG.PCW_UIPARAM_DDR_DQS_TO_CLK_DELAY_3"        ->"-0.100",
    "CONFIG.PCW_UIPARAM_DDR_CLOCK_0_LENGTH_MM"         ->"18.8",
    "CONFIG.PCW_UIPARAM_DDR_CLOCK_0_PACKAGE_LENGTH"    ->"80.4535",
    "CONFIG.PCW_UIPARAM_DDR_CLOCK_0_PROPOGATION_DELAY" ->"160",
    "CONFIG.PCW_UIPARAM_DDR_CLOCK_1_LENGTH_MM"         ->"18.8",
    "CONFIG.PCW_UIPARAM_DDR_CLOCK_1_PACKAGE_LENGTH"    ->"80.4535",
    "CONFIG.PCW_UIPARAM_DDR_CLOCK_1_PROPOGATION_DELAY" ->"160",
    "CONFIG.PCW_UIPARAM_DDR_CLOCK_2_LENGTH_MM"         ->"18.8",
    "CONFIG.PCW_UIPARAM_DDR_CLOCK_2_PACKAGE_LENGTH"    ->"80.4535",
    "CONFIG.PCW_UIPARAM_DDR_CLOCK_2_PROPOGATION_DELAY" ->"160",
    "CONFIG.PCW_UIPARAM_DDR_CLOCK_3_LENGTH_MM"         ->"18.8",
    "CONFIG.PCW_UIPARAM_DDR_CLOCK_3_PACKAGE_LENGTH"    ->"80.4535",
    "CONFIG.PCW_UIPARAM_DDR_CLOCK_3_PROPOGATION_DELAY" ->"160",
    "CONFIG.PCW_UIPARAM_DDR_DQS_0_LENGTH_MM"           ->"22.8",
    "CONFIG.PCW_UIPARAM_DDR_DQS_0_PACKAGE_LENGTH"      ->"105.056",
    "CONFIG.PCW_UIPARAM_DDR_DQS_0_PROPOGATION_DELAY"   ->"160",
    "CONFIG.PCW_UIPARAM_DDR_DQS_1_LENGTH_MM"           ->"27.9",
    "CONFIG.PCW_UIPARAM_DDR_DQS_1_PACKAGE_LENGTH"      ->"66.904",
    "CONFIG.PCW_UIPARAM_DDR_DQS_1_PROPOGATION_DELAY"   ->"160",
    "CONFIG.PCW_UIPARAM_DDR_DQS_2_LENGTH_MM"           ->"22.9",
    "CONFIG.PCW_UIPARAM_DDR_DQS_2_PACKAGE_LENGTH"      ->"89.1715",
    "CONFIG.PCW_UIPARAM_DDR_DQS_2_PROPOGATION_DELAY"   ->"160",
    "CONFIG.PCW_UIPARAM_DDR_DQS_3_LENGTH_MM"           ->"29.4",
    "CONFIG.PCW_UIPARAM_DDR_DQS_3_PACKAGE_LENGTH"      ->"113.63",
    "CONFIG.PCW_UIPARAM_DDR_DQS_3_PROPOGATION_DELAY"   ->"160",
    "CONFIG.PCW_UIPARAM_DDR_DQ_0_LENGTH_MM"            ->"22.8",
    "CONFIG.PCW_UIPARAM_DDR_DQ_0_PACKAGE_LENGTH"       ->"98.503",
    "CONFIG.PCW_UIPARAM_DDR_DQ_0_PROPOGATION_DELAY"    ->"160",
    "CONFIG.PCW_UIPARAM_DDR_DQ_1_LENGTH_MM"            ->"27.9",
    "CONFIG.PCW_UIPARAM_DDR_DQ_1_PACKAGE_LENGTH"       ->"68.5855",
    "CONFIG.PCW_UIPARAM_DDR_DQ_1_PROPOGATION_DELAY"    ->"160",
    "CONFIG.PCW_UIPARAM_DDR_DQ_2_LENGTH_MM"            ->"22.9",
    "CONFIG.PCW_UIPARAM_DDR_DQ_2_PACKAGE_LENGTH"       ->"90.295",
    "CONFIG.PCW_UIPARAM_DDR_DQ_2_PROPOGATION_DELAY"    ->"160",
    "CONFIG.PCW_UIPARAM_DDR_DQ_3_LENGTH_MM"            ->"29.4",
    "CONFIG.PCW_UIPARAM_DDR_DQ_3_PACKAGE_LENGTH"       ->"103.977",
    "CONFIG.PCW_UIPARAM_DDR_DQ_3_PROPOGATION_DELAY"    ->"160")

}
