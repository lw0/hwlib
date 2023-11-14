package hwlib.xilinx.board

import spinal.core._

import hwlib.xilinx.tools.{XdcInfo, Pin, Pins, Std, Stds, Pull, Clock, Rename}


object Nexys4DDR extends Board {
  def part = "xc7a100tcsg324-1"
  def board = Some("digilentinc.com:nexys4_ddr:part0:1.1")
  def ports = Map(
    "CLK100M" -> XdcInfo(
      // Sch=CLK100MHZ
      Pin("E3"), Std("LVCMOS33"), Clock(10.0)),

    "RST_N" -> XdcInfo(
      // Sch=CPU_RESETN
      Pin("C12"), Std("LVCMOS33")),

    "SWITCH" -> XdcInfo(
      // Sch=SW{0..15}
      Pins("J15",      "L16",      "M13",      "R15",
           "R17",      "T18",      "U18",      "R13",
           "T8",       "U8",       "R16",      "T13",
           "H6",       "U12",      "U11",      "V10"),
      Stds("LVCMOS33", "LVCMOS33", "LVCMOS33", "LVCMOS33",
           "LVCMOS33", "LVCMOS33", "LVCMOS33", "LVCMOS33",
           "LVCMOS18", "LVCMOS18", "LVCMOS33", "LVCMOS33",
           "LVCMOS33", "LVCMOS33", "LVCMOS33", "LVCMOS33")),

    "LED" -> XdcInfo(
      // Sch=LED{0..15}
      Pins("H17", "K15", "J13", "N14", "R18", "V17", "U17", "U16",
           "V16", "T15", "U14", "T16", "V15", "V14", "V12", "V11"),
       Std("LVCMOS33")),

    "RGBR" -> XdcInfo.multi(
      "red" -> Seq(
        // Sch=LED16_R
        Pin("N15"), Std("LVCMOS33")),
      "green" -> Seq(
        // Sch=LED16_G
        Pin("M16"), Std("LVCMOS33")),
      "blue" -> Seq(
        // Sch=LED16_B
        Pin("R12"), Std("LVCMOS33"))),

    "RGBL" -> XdcInfo.multi(
      "red" -> Seq(
        // Sch=LED17_R
        Pin("N16"), Std("LVCMOS33")),
      "green" -> Seq(
        // Sch=LED17_G
        Pin("R11"), Std("LVCMOS33")),
      "blue" -> Seq(
        // Sch=LED17_B
       Pin("G14"), Std("LVCMOS33"))),

    "DISPLAY" -> XdcInfo.multi(
      "segment" -> Seq(
        // Sch={CA, CB, CC, CD, CE, CF, CG, DP}
        Pins("T10", "R10", "K16", "K13", "P15", "T11", "L18", "H15"),
         Std("LVCMOS33")),
      "digit" -> Seq(
        // Sch=AN{0..7}
        Pins("J17", "J18", "T9", "J14", "P14", "T14", "K2", "U13"),
         Std("LVCMOS33"))),

    "BUTTON" -> XdcInfo.multi(
      "center" -> Seq(
        // Sch=BTNC
        Pin("N17"), Std("LVCMOS33")),
      "up" -> Seq(
        // Sch=BTNU
        Pin("M18"), Std("LVCMOS33")),
      "left" -> Seq(
        // Sch=BTNL
        Pin("P17"), Std("LVCMOS33")),
      "right" -> Seq(
        // Sch=BTNR
        Pin("M17"), Std("LVCMOS33")),
      "down" -> Seq(
        // Sch=BTND
        Pin("P18"), Std("LVCMOS33"))),

    "SERIAL" -> XdcInfo.multi(
      "tx" -> Seq(
        // Sch=UART_TXD_IN
        Pin("C4"), Std("LVCMOS33")),
      "rx" -> Seq(
        // Sch=UART_RXD_OUT
        Pin("D4"), Std("LVCMOS33")),
      "cts" -> Seq(
        // Sch=UART_CTS
        Pin("D3"), Std("LVCMOS33")),
      "rts" -> Seq(
        // Sch=UART_RTS
        Pin("E5"), Std("LVCMOS33"))),

    "PS2" -> XdcInfo.multi(
      "clk" -> Seq(
        // Sch=PS2_CLK
        Pin("F4"), Std("LVCMOS33")),
      "dat" -> Seq(
        // Sch=PS2_DATA
        Pin("B2"), Std("LVCMOS33"))),

    "VGA" -> XdcInfo.multi(
      "red" -> Seq(
        // Sch=VGA_R{0..3}
        Pins("A3", "B4", "C5", "A4"),
         Std("LVCMOS33")),
      "green" -> Seq(
        // Sch=VGA_G{0..3}
        Pins("C6", "A5", "B6", "A6"),
         Std("LVCMOS33")),
      "blue" -> Seq(
        // Sch=VGA_B{0..3}
        Pins("B7", "C7", "D7", "D8"),
         Std("LVCMOS33")),
      "hsync" -> Seq(
        // Sch=VGA_HS
        Pin("B11"), Std("LVCMOS33")),
      "vsync" -> Seq(
        // Sch=VGA_VS
        Pin("B12"), Std("LVCMOS33"))),

    "SD" -> XdcInfo.multi(
      "reset" -> Seq(
        // Sch=SD_RESET
        Pin("E2"), Std("LVCMOS33")),
      "cd" -> Seq(
        // Sch=SD_CD
        Pin("A1"), Std("LVCMOS33")),
      "clk" -> Seq(
        // Sch=SD_SCK
        Pin("B1"), Std("LVCMOS33")),
      "cmd" -> Seq(
        // Sch=SD_CMD
        Pin("C1"), Std("LVCMOS33")),
      "dat" -> Seq(
        // Sch=SD_DAT{0..3}
        Pins("C2", "E1", "F1", "D2"),
         Std("LVCMOS33"))),

    "SDSPI" -> XdcInfo.multi(
      "reset" -> Seq(
        // Sch=SD_RESET
        Pin("E2"), Std("LVCMOS33")),
      "cd" -> Seq(
        // Sch=SD_CD
        Pin("A1"), Std("LVCMOS33")),
      "sclk" -> Seq(
        // Sch=SD_SCK
        Pin("B1"), Std("LVCMOS33")),
      "mosi" -> Seq(
        // Sch=SD_CMD
        Pin("C1"), Std("LVCMOS33")),
      "miso" -> Seq(
        // Sch=SD_DAT0
        Pin("C2"), Std("LVCMOS33")),
      "ssel_n" -> Seq(
        // Sch=SD_DAT3
        Pin("D2"), Std("LVCMOS33"))),

    "MIC" -> XdcInfo.multi(
      "clk" -> Seq(
        // Sch=M_CLK
        Pin("J5"), Std("LVCMOS33")),
      "data" -> Seq(
        // Sch=M_DATA
        Pin("H5"), Std("LVCMOS33")),
      "lrsel" -> Seq(
        // Sch=M_LRSEL
        Pin("F5"), Std("LVCMOS33"))),

    "AUD" -> XdcInfo.multi(
      "pwm" -> Seq(
        // Sch=AUD_PWM
        Pin("A11"), Std("LVCMOS33")),
      "enable" -> Seq(
        // Sch=AUD_SD (opamp not shutdown)
        Pin("D12"), Std("LVCMOS33"))),

    "ETH" -> XdcInfo.multi(
      "mdc" -> Seq(
        // Sch=ETH_MDC
        Pin("C9"), Std("LVCMOS33")),
      "mdio" -> Seq(
        // Sch=ETH_MDIO
        Pin("A9"), Std("LVCMOS33")),
      "rst_n" -> Seq(
        // Sch=ETH_RSTN
        Pin("B3"), Std("LVCMOS33")),
      "cdsdv" -> Seq(
        // Sch=ETH_CRSDV
        Pin("D9"), Std("LVCMOS33")),
      "rxerr" -> Seq(
        // Sch=ETH_RXERR
        Pin("C10"), Std("LVCMOS33")),
      "rxd" -> Seq(
        // Sch=ETH_RXD{0..1}
        Pins("C11", "D10"),
         Std("LVCMOS33")),
      "txen" -> Seq(
        // Sch=ETH_TXEN
        Pin("B9"), Std("LVCMOS33")),
      "txd" -> Seq(
        // Sch=ETH_TXD{0..1}
        Pins("A10", "A8"),
         Std("LVCMOS33")),
      "refclk" -> Seq(
        // Sch=ETH_REFCLK
        Pin("D5"), Std("LVCMOS33")),
      "int_n" -> Seq(
        // Sch=ETH_INTN
        Pin("B8"), Std("LVCMOS33"))),

    "FLASH" -> XdcInfo.multi(
      "ssel_n" -> Seq(
        // Sch=QSPI_CSN
        Pin("L13"), Std("LVCMOS33")),
      "dq" -> Seq(
        // Sch=QSPI_DQ{0..3}
        Pins("K17", "K18", "L14", "M14"),
         Std("LVCMOS33"))),

    "ACCEL" -> XdcInfo.multi(
      "ssel_n" -> Seq(
        // Sch=ACL_CSN
        Pin("D15"), Std("LVCMOS33")),
      "sclk" -> Seq(
        // Sch=ACL_SCLK
        Pin("F15"), Std("LVCMOS33")),
      "mosi" -> Seq(
        // Sch=ACL_MOSI
        Pin("F14"), Std("LVCMOS33")),
      "miso" -> Seq(
        // Sch=ACL_MISO
        Pin("E15"), Std("LVCMOS33")),
      "int1" -> Seq(
        // Sch=ACL_INT1
        Pin("B13"), Std("LVCMOS33")),
      "int2" -> Seq(
        // Sch=ACL_INT2
        Pin("C16"), Std("LVCMOS33"))),

    "TEMP" -> XdcInfo.multi(
      "scl" -> Seq(
        // Sch=TMP_SCL
        Pin("C14"), Std("LVCMOS33")),
      "sda" -> Seq(
        // Sch=TMP_SDA
        Pin("C15"), Std("LVCMOS33")),
      "int" -> Seq(
        // Sch=TMP_INT
        Pin("D13"), Std("LVCMOS33")),
      "crt" -> Seq(
        // Sch=TMP_CT
        Pin("B14"), Std("LVCMOS33"))),

    "PMODA" -> XdcInfo(
      // Sch=JA{1..4, 7..10}
      Pins("C17", "D18", "E18", "G17", "D17", "E17", "F18", "G18"),
       Std("LVCMOS33")),

    "PMODB" -> XdcInfo(
      // Sch=JB{1..4, 7..10}
      Pins("D14", "F16", "G16", "H14", "E16", "F13", "G13", "H16"),
       Std("LVCMOS33")),

    "PMODC" -> XdcInfo(
      // Sch=JC[1..4, 7..10]
      Pins("K1", "F6", "J2", "G6", "E7", "J3", "J4", "E6"),
       Std("LVCMOS33")),

    "PMODD" -> XdcInfo(
      // Sch=JD[1..4, 7..10]
      Pins("H4", "H1", "G1", "G3", "H2", "G4", "G2", "F3"),
       Std("LVCMOS33")),

    "XADC" -> XdcInfo.multi(
      "n" -> Seq(
        // Sch=XADC{1..4}_N
        Pins("A14", "A16", "B17", "A18"),
         Std("LVDS")),
      "p" -> Seq(
        // Sch=XADC{1..4}_P
        Pins("A13", "A15", "B16", "B18"),
         Std("LVDS"))),

    "DDR2" -> XdcInfo.multi(
      "dq" -> Seq(
        // Sch=DDR_DQ{0..15}
        Pins("R7", "V6", "R8", "U7", "V7", "R6", "U6", "R5",
             "T5", "U3", "V5", "U4", "V4", "T4", "V1", "T3"),
         Std("SSTL18_II"), Rename("ddr2_dq")),
      "dm" -> Seq(
        // Sch=DDR_{L, U}DM
        Pins("T6", "U1"),
         Std("SSTL18_II"), Rename("ddr2_dm")),
      "dqs_n" -> Seq(
        // Sch=DDR_{L, U}DQS_N
        Pins("V9", "V2"),
         Std("DIFF_SSTL18_II"), Rename("ddr2_dqs_n")),
      "dqs_p" -> Seq(
        // Sch=DDR_{L, U}DQS_P
        Pins("U9", "U2"),
         Std("DIFF_SSTL18_II"), Rename("ddr2_dqs_p")),
      "addr" -> Seq(
        // Sch=DDR_A{0..12}
        Pins("M4", "P4", "M6", "T1", "L3", "P5", "M2", "N1",
             "L4", "N5", "R2", "K5", "N6"),
         Std("SSTL18_II"), Rename("ddr2_addr")),
      "ba" -> Seq(
        // Sch=DDR_BA{0..2}
        Pins("P2", "P3", "R1"),
         Std("SSTL18_II"), Rename("ddr2_ba")),
      "ck_n" -> Seq(
        // Sch=DDR_CK_N
        Pins("L5"),
         Std("DIFF_SSTL18_II"), Rename("ddr2_ck_p")),
      "ck_p" -> Seq(
        // Sch=DDR_CK_P
        Pins("L6"),
         Std("DIFF_SSTL18_II"), Rename("ddr2_ck_n")),
      "cke" -> Seq(
        // Sch=DDR_CKE
        Pins("M1"),
         Std("SSTL18_II"), Rename("ddr2_cke")),
      "odt" -> Seq(
        // Sch=DDR_ODT
        Pins("M3"),
         Std("SSTL18_II"), Rename("ddr2_odt")),
      "cs_n" -> Seq(
        // Sch=DDR_CS
        Pins("K6"),
         Std("SSTL18_II"), Rename("ddr2_cs_n")),
      "ras_n" -> Seq(
        // Sch=DDR_RAS
        Pin("N4"), Std("SSTL18_II"), Rename("ddr2_ras_n")),
      "cas_n" -> Seq(
        // Sch=DDR_CAS
        Pin("L1"), Std("SSTL18_II"), Rename("ddr2_cas_n")),
      "we_n" -> Seq(
        // Sch=DDR_WE
        Pin("N2"), Std("SSTL18_II"), Rename("ddr2_we_n"))))
}

