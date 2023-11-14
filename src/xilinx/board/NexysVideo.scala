package hwlib.xilinx.board

import spinal.core._

import hwlib.xilinx.tools.{XdcInfo, Pin, Pins, Std, Stds, Pull, Clock, Rename}


abstract class NexysVideoVAdj {
  def iostd : String
  def setting : Int
}

object VAdj1V2 extends NexysVideoVAdj {
  def iostd = "LVCMOS12"
  def setting = 0
}
object VAdj1V8 extends NexysVideoVAdj {
  def iostd = "LVCMOS18"
  def setting = 1
}
object VAdj2V5 extends NexysVideoVAdj {
  def iostd = "LVCMOS25"
  def setting = 2
}
object VAdj3V3 extends NexysVideoVAdj {
  def iostd = "LVCMOS33"
  def setting = 3
}

case class NexysVideo(vadj : NexysVideoVAdj = VAdj1V2) extends Board {
  def part = "xc7a200tsbg484-1"
  def board = Some("digilentinc.com:nexys_video:part0:1.2")
  def selectVAdj = B(vadj.setting, 2 bits)
  def ports = Map(
    "CLK100M" -> XdcInfo(
      // Sch=SYSCLK
      Pin("R4"), Std("LVCMOS33"), Clock(10.0)),

    "RST_N" -> XdcInfo(
      // Sch=CPU_RESETN
      Pin("G4"), Std("LVCMOS15")),

    "SWITCH" -> XdcInfo(
      // Sch=SW{0..7}
      Pins("E22", "F21", "G21", "G22", "H17", "J16", "K13", "M17"),
       Std(vadj.iostd)),

    "BUTTON" -> XdcInfo.multi(
      "center" -> Seq(
        // Sch=BTNC
        Pin("N17"), Std(vadj.iostd)),
      "up" -> Seq(
        // Sch=BTNU
        Pin("M18"), Std(vadj.iostd)),
      "left" -> Seq(
        // Sch=BTNL
        Pin("P17"), Std(vadj.iostd)),
      "right" -> Seq(
        // Sch=BTNR
        Pin("M17"), Std(vadj.iostd)),
      "down" -> Seq(
        // Sch=BTND
        Pin("P18"), Std(vadj.iostd))),

    "LED" -> XdcInfo(
      // Sch=LED{0..7}
      Pins("T14", "T15", "T16", "U16", "V15", "W16", "W15", "Y13"),
       Std("LVCMOS25")),

    "OLED" -> XdcInfo.multi(
      "dc" -> Seq(
        // Sch=OLED_DC
        Pin("W22"), Std("LVCMOS33")),
      "res" -> Seq(
        // Sch=OLED_RES
        Pin("U21"), Std("LVCMOS33")),
      "sclk" -> Seq(
        // Sch=OLED_SCLK
        Pin("W21"), Std("LVCMOS33")),
      "mosi" -> Seq(
        // Sch=OLED_SDIN
        Pin("Y22"), Std("LVCMOS33")),
      "vbat" -> Seq(
        // Sch=OLED_VBAT
        Pin("P20"), Std("LVCMOS33")),
      "vdd" -> Seq(
        // Sch=OLED_VDD
        Pin("V22"), Std("LVCMOS33"))),

    "SERIAL" -> XdcInfo.multi(
      "tx" -> Seq(
        // Sch=UART_TXD_IN
        Pin("V18"),  Std("LVCMOS33")),
      "rx" -> Seq(
        // Sch=UART_RXD_OUT
        Pin("AA19"), Std("LVCMOS33"))),

    "USB_FIFO" -> XdcInfo.multi(
      "clk" -> Seq(
        // Sch=PROG_CLKO
        Pin("Y18"), Std("LVCMOS33"), Clock(16.666)),
      "data" -> Seq(
        // Sch=PROG_D{0..7}
        Pins("U20", "P14", "P15", "U17", "R17", "P16", "R18", "N14"),
         Std("LVCMOS33")),
      "rxf_n" -> Seq(
        // Sch=PROG_RXEN
        Pin("N17"), Std("LVCMOS33")),
      "txe_n" -> Seq(
        // Sch=PROG_TXEN
        Pin("Y19"), Std("LVCMOS33")),
      "oe_n" -> Seq(
        // Sch=PROG_OEN
        Pin("V17"), Std("LVCMOS33")),
      "rd_n" -> Seq(
        // Sch=PROG_RDN
        Pin("P19"), Std("LVCMOS33")),
      "wr_n" -> Seq(
        // Sch=PROG_WRN
        Pin("R19"), Std("LVCMOS33")),
      "siwu_n" -> Seq(
        // Sch=PROG_SIWUN
        Pin("P17"), Std("LVCMOS33")),
      "spien" -> Seq(
        // Sch=PROG_SPIEN
        Pin("R14"), Std("LVCMOS33"))),

    "USB_SPI" -> XdcInfo.multi(
      "sclk" -> Seq(
        // Sch=PROG_D0/SCK
        Pin("U20"), Std("LVCMOS33")),
      "mosi" -> Seq(
        // Sch=PROG_D1/MOSI
        Pin("P14"), Std("LVCMOS33")),
      "miso" -> Seq(
        // Sch=PROG_D2/MISO
        Pin("P15"), Std("LVCMOS33")),
      "ssel_n" -> Seq(
        // Sch=PROG_D3/SS
        Pin("U17"), Std("LVCMOS33")),
      "siwu_n" -> Seq(
        // Sch=PROG_SIWUN
        Pin("P17"), Std("LVCMOS33")),
      "spien" -> Seq(
        // Sch=PROG_SPIEN
        Pin("R14"), Std("LVCMOS33"))),

    "I2C" -> XdcInfo.multi(
      // Audio Codec @0x3B
      // EEPROM with MAC Address @0x57
      "scl" -> Seq(
        // Sch=SCL
        Pin("C14"), Std("LVCMOS33")),
      "sda" -> Seq(
        // Sch=SDA
        Pin("C15"), Std("LVCMOS33"))),

    "AUDIO" -> XdcInfo.multi(
      // Control via "I2C" @0x3B
      "mclk" -> Seq(
        // Sch=AC_MCLK
        Pin("U6"), Std("LVCMOS33")),
      "bclk" -> Seq(
        // Sch=AC_BCLK
        Pin("T5"), Std("LVCMOS33")),
      "plysdata" -> Seq(
        // Sch=AC_DAC_SDATA
        Pin("W6"), Std("LVCMOS33")),
      "recsdata" -> Seq(
        // Sch=AC_ADC_SDATA
        Pin("T4"), Std("LVCMOS33")),
      "lrclk" -> Seq(
        // Sch=AC_LRCLK
        Pin("U5"), Std("LVCMOS33"))),

    "FAN_PWM" -> XdcInfo(
      // Sch=FAN_PWM
      Pin("U15"), Std("LVCMOS25")),

    "PS2" -> XdcInfo.multi(
      "clk" -> Seq(
        // Sch=PS2_CLK
        Pin("W17"), Std("LVCMOS33"), Pull("PULLUP")),
      "dat" -> Seq(
        // Sch=PS2_DATA
        Pin("N13"), Std("LVCMOS33"), Pull("PULLUP"))),

    "SD" -> XdcInfo.multi(
      "reset" -> Seq(
        // Sch=SD_RESET
        Pin("V20"), Std("LVCMOS33")),
      "cd" -> Seq(
        // Sch=SD_CD
        Pin("T18"), Std("LVCMOS33")),
      "clk" -> Seq(
        // Sch=SD_CCLK
        Pin("W19"), Std("LVCMOS33")),
      "cmd" -> Seq(
        // Sch=SD_CMD
        Pin("W20"), Std("LVCMOS33")),
      "dat" -> Seq(
        // Sch=SD_D{0..3}
        Pins("V19", "T21", "T20", "U18"),
         Std("LVCMOS33"))),

    "SDSPI" -> XdcInfo.multi(
      "reset" -> Seq(
        // Sch=SD_RESET
        Pin("V20"), Std("LVCMOS33")),
      "cd" -> Seq(
        // Sch=SD_CD
        Pin("T18"), Std("LVCMOS33")),
      "sclk" -> Seq(
        // Sch=SD_CCLK
        Pin("W19"), Std("LVCMOS33")),
      "mosi" -> Seq(
        // Sch=SD_CMD
        Pin("W20"), Std("LVCMOS33")),
      "miso" -> Seq(
        // Sch=SD_D0
        Pin("V19"), Std("LVCMOS33")),
      "ssel_n" -> Seq(
        // Sch=SD_D3
        Pin("U18"), Std("LVCMOS33"))),

    "ETH" -> XdcInfo.multi(
      // EEPROM with MAC Address on "I2C" @0x57
      "mdc" -> Seq(
        // Sch=ETH_MDC
        Pin("AA16"), Std("LVCMOS25")),
      "mdio" -> Seq(
        // Sch=ETH_MDIO
        Pin("Y16"),  Std("LVCMOS25")),
      "rst_b" -> Seq(
        // Sch=ETH_RST_B
        Pin("U7"),   Std("LVCMOS25")),
      "int_b" -> Seq(
        // Sch=ETH_INT_B
        Pin("Y14"),  Std("LVCMOS25")),
      "pme_b" -> Seq(
        // Sch=ETH_PME_B
        Pin("W14"),  Std("LVCMOS25")),
      "rxck" -> Seq(
        // Sch=ETH_RXCK
        Pin("V13"), Std("LVCMOS25")),
      "rxctl" -> Seq(
        // Sch=ETH_RXCTL
        Pin("W10"), Std("LVCMOS25")),
      "rxd" -> Seq(
        // Sch=ETH_RXD{0..3}
        Pins("AB16", "AA15", "AB15", "AB11"),
         Std("LVCMOS25")),
      "txck" -> Seq(
        // Sch=ETH_TXCK
        Pin("AA14"), Std("LVCMOS25")),
      "txctl" -> Seq(
        // Sch=ETH_TXCTL
        Pin("V10"), Std("LVCMOS25")),
      "txd" -> Seq(
        // Sch=ETH_TXD{0..3}
        Pins("Y12", "W12", "W11", "Y11"),
         Std("LVCMOS25"))),

    "HDMI_SINK" -> XdcInfo.multi(
      "enable" -> Seq(
        // Sch=hdmi_rx_txen
        Pin("R3"),   Std("LVCMOS33")),
      "hpa" -> Seq(
        // Sch=hdmi_rx_hpa
        Pin("AB12"), Std("LVCMOS25")),
      "scl" -> Seq(
        // Sch=hdmi_rx_scl
        Pin("Y4"),   Std("LVCMOS33")),
      "sda" -> Seq(
        // Sch=hdmi_rx_sda
        Pin("AB5"),  Std("LVCMOS33")),
      "cec" -> Seq(
        // Sch=hdmi_rx_cec
        Pin("AA5"),  Std("LVCMOS33")),
      "clk_n" -> Seq(
        // Sch=hdmi_rx_clk_n
        Pin("W4"),   Std("TMDS_33")),
      "clk_p" -> Seq(
        // Sch=hdmi_rx_clk_p
        Pin("V4"),   Std("TMDS_33")),
      "dat_n" -> Seq(
        // Sch=hdmi_rx_n{0..2}
        Pins("AA3", "Y2", "V2"),
         Std("TMDS_33")),
      "dat_p" -> Seq(
        // Sch=hdmi_rx_p{0..2}
        Pins("Y3", "W2", "U2"),
         Std("TMDS_33"))),

    "HDMI_SOURCE" -> XdcInfo.multi(
      "hpd" -> Seq(
        // Sch=hdmi_tx_hpd
        Pin("AB13"), Std("LVCMOS25")),
      "scl" -> Seq(
        // Sch=hdmi_tx_rscl
        Pin("U3"),   Std("LVCMOS33")),
      "sda" -> Seq(
        // Sch=hdmi_tx_rsda
        Pin("V3"),   Std("LVCMOS33")),
      "cec" -> Seq(
        // Sch=hdmi_tx_cec
        Pin("AA4"),  Std("LVCMOS33")),
      "clk_n" -> Seq(
        // Sch=hdmi_tx_clk_n
        Pin("U1"),   Std("TMDS_33")),
      "clk_p" -> Seq(
        // Sch=hdmi_tx_clk_p
        Pin("T1"),   Std("TMDS_33")),
      "dat_n" -> Seq(
        // Sch=hdmi_tx_n{0..2}
        Pins("Y1", "AB1", "AB2"),
         Std("TMDS_33")),
      "dat_p" -> Seq(
        // Sch=hdmi_tx_p{0..2}
        Pins("W1", "AA1", "AB3"),
         Std("TMDS_33"))),

    "DPORT" -> XdcInfo.multi(
      // refclk -> REFCLK0 Pins("E6", "F6") Sch=GTP_CLK_{N,P}
      // lane 0 -> MGTP0 Pins("A4", "B4") Sch=DP_TX_LANE0_{N,P}
      // lane 1 -> MGTP1 Pins("C5", "D5") Sch=DP_TX_LANE1_{N,P}
      "refclk_n" -> Seq(
        // Sch=gtp_clk_n
        Pin("E6")),
      "refclk_p" -> Seq(
        // Sch=gtp_clk_p
        Pin("F6"), Clock(6.4)),
      "hpd" -> Seq(
        // Sch=dp_tx_hpd
        Pin("N15"),  Std("LVCMOS33")),
      "aux_i_n" -> Seq(
        // Sch=dp_tx_aux_n
        Pin("AB10"), Std("TMDS_33")),
      "aux_o_n" -> Seq(
        // Sch=dp_tx_aux_n
        Pin("AA11"), Std("TMDS_33")),
      "aux_i_p" -> Seq(
        // Sch=dp_tx_aux_p
        Pin("AA9"),  Std("TMDS_33")),
      "aux_o_p" -> Seq(
        // Sch=dp_tx_aux_p
        Pin("AA10"), Std("TMDS_33")),
      "aux_n" -> Seq(
        // Sch=dp_tx_aux_n
        Pin("AB10"), Std("TMDS_33")),
      "aux_p" -> Seq(
        // Sch=dp_tx_aux_p
        Pin("AA9"),  Std("TMDS_33"))),


    "FLASH" -> XdcInfo.multi(
      "ssel_n" -> Seq(
        // Sch=QSPI_CS
        Pin("T19"), Std("LVCMOS33")),
      "dq" -> Seq(
        // Sch=QSPI_DQ{0..3}
        Pins("P22", "R22", "P21", "R21"),
         Std("LVCMOS33"))),

    "PMODA" -> XdcInfo(
      // Sch=JA{1..4, 7..10}
      Pins("AB22", "AB21", "AB20", "AB18", "Y21", "AA21", "AA20", "AA18"),
       Std("LVCMOS33")),

    "PMODB" -> XdcInfo.multi(
      "n" -> Seq(
        // Sch=JB{1..4}_N
        Pins("V8", "W7", "Y9", "Y7"),
         Std("LVCMOS33")),
      "p" -> Seq(
        // Sch=JB{1..4}_P
        Pins("V9", "V7", "W9", "Y8"),
         Std("LVCMOS33"))),

    "PMODC" -> XdcInfo.multi(
      "n" -> Seq(
        // Sch=JC{1..4}_N
        Pins("AA6", "AB8", "T6", "AB6"),
         Std("LVCMOS33")),
      "p" -> Seq(
        // Sch=JC{1..4}_P
        Pins("Y6", "AA8", "R6", "AB7"),
         Std("LVCMOS33"))),

    "XADC" -> XdcInfo.multi(
      "n" -> Seq(
        // Sch=XADC{1..4}_N
        Pins("H14", "G13", "G16", "H15"),
         Std("LVCMOS33")),
      "p" -> Seq(
        // Sch=XADC{1..4}_P
        Pins("J14", "H13", "G15", "J15"),
         Std("LVCMOS33"))),

    "VADJ" -> XdcInfo.multi(
      "enable" -> Seq(
        // Sch=VADJ_EN
        Pin("V14"), Std("LVCMOS25")),
      "select" -> Seq(
        // Sch=SET_VADJ{0..1}
        Pins("AA13", "AB17"), Std("LVCMOS25"))),

    "FMC" -> XdcInfo.multi(
      // gtlane -> MGTP3 Pins("C9", "D9", "C7", "D7") Sch=FMC_MGT_{M2C_{N,P},C2M_{N,P}}
      // refclk -> REFCLK1 Pins("E10", "F10") Sch=FMC_MGT_CLK_{N,P}
      "refclk_n" -> Seq(
        // Sch=FMC_MGT_CLK_N
        Pin("E10")),
      "refclk_p" -> Seq(
        // Sch=FMC_MGT_CLK_P
        Pin("F10"), Clock(6.4)),
      "clk_n" -> Seq(
        // Sch=FMC_CLK{0..1}_M2C_N
        Pins("H19", "C19"),
         Std(vadj.iostd)),
      "clk_p" -> Seq(
        // Sch=FMC_CLK{0..1}_M2C_P
        Pins("J19", "C18"),
         Std(vadj.iostd)),
      "lane_n" -> Seq(
        // Sch=FMC_LA{{00..01}_CC,{02..16},{17..18}_CC,{19..33}}_N
        Pins("K19", "J21",
             "L18", "N19", "M20", "L21", "M22", "L13", "M16", "G20",
                    "K22", "L15", "L20", "J17", "H22", "K16", "G18",
             "B18", "C17",
             "A19", "F20", "D19", "D21", "A21", "B16", "E17", "E18",
                    "A20", "B13", "C15", "A14", "E14", "A16", "F14"),
         Std(vadj.iostd)),
      "lane_p" -> Seq(
        // Sch=FMC_LA{{00..01}_CC,{02..16},{17..18}_CC,{19..33}}_P
        Pins("K18", "J20",
             "M18", "N18", "N20", "M21", "N22", "M13", "M15", "H20",
                    "K21", "L14", "L19", "K17", "J22", "L16", "G17",
             "B17", "D17",
             "A18", "F19", "E19", "E21", "B21", "B15", "F16", "F18",
                    "B20", "C13", "C14", "A13", "E13", "A15", "F13"),
         Std(vadj.iostd))),

    "DDR3" -> XdcInfo.multi(
      "dq" -> Seq(
        // Sch=DDR3_DQ{0..15}
        Pins("G2", "H4", "H5", "J1", "K1", "H3", "H2", "J5",
             "E3", "B2", "F3", "D2", "C2", "A1", "E2", "B1"),
        Rename("ddr3_dq")),
      "dm" -> Seq(
        // Sch=DDR3_{L, U}DM
        Pins("G3", "F1"), Rename("ddr3_dm")),
      "dqs_n" -> Seq(
        // Sch=DDR3_DQS{0,1}_N
        Pins("J2", "D1"), Rename("ddr3_dqs_n")),
      "dqs_p" -> Seq(
        // Sch=DDR3_DQS{0,1}_P
        Pins("K2", "E1"), Rename("ddr3_dqs_p")),
      "addr" -> Seq(
        // Sch=DDR3_ADDR{0..14}
        Pins("M2", "M5", "M3", "M1", "L6", "P1", "N3", "N2",
             "M6", "R1", "L5", "N5", "N4", "P2", "P6"),
        Rename("ddr3_addr")),
      "ba" -> Seq(
        // Sch=DDR3_BA{0..2}
        Pins("L3", "K6", "L4"), Rename("ddr2_ba")),
      "ck_n" -> Seq(
        // Sch=DDR3_CLK0_N
        Pins("P4"), Rename("ddr3_ck_p")),
      "ck_p" -> Seq(
        // Sch=DDR3_CLK0_P
        Pins("P5"), Rename("ddr3_ck_n")),
      "cke" -> Seq(
        // Sch=DDR3_CKE0
        Pins("J6"), Rename("ddr3_cke")),
      "odt" -> Seq(
        // Sch=DDR3_ODT
        Pins("K4"), Rename("ddr3_odt")),
      "ras_n" -> Seq(
        // Sch=DDR3_RAS
        Pin("J4"), Rename("ddr3_ras_n")),
      "cas_n" -> Seq(
        // Sch=DDR3_CAS
        Pin("K3"), Rename("ddr3_cas_n")),
      "we_n" -> Seq(
        // Sch=DDR3_WE
        Pin("L1"), Rename("ddr3_we_n"))))
  /*
    ## Configuration options, can be used for all designs
    set_property CONFIG_VOLTAGE 3.3 [current_design]
    set_property CFGBVS VCCO [current_design]
  */
}

