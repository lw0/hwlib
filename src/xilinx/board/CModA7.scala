package hwlib.xilinx.board

import spinal.core._

import hwlib.xilinx.tools.{XdcInfo, Pin, Pins, Std, Stds, Pull, Clock, Rename}


object CModA735T extends Board {
  def part = "xc7a35tcpg236-1"
  def board = Some("digilentinc.com:cmod_a7-35t:part0:1.1")
  def ports = Map(
    "CLK12M" -> XdcInfo(
      // Sch=GCLK
      Pin("L17"), Std("LVCMOS33"), Clock(83.33)),

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
      // Sch=LED{1..2}
      Pins("A17", "C16"),
       Std("LVCMOS33")),

    "RGB" -> XdcInfo.multi(
      "red" -> Seq(
        // Sch=LED0_R
        Pin("C17"), Std("LVCMOS33")),
      "green" -> Seq(
        // Sch=LED0_G
        Pin("B16"), Std("LVCMOS33")),
      "blue" -> Seq(
        // Sch=LED0_B
        Pin("B17"), Std("LVCMOS33"))),

    "BUTTON" -> XdcInfo(
      // Sch=BTN{0..1}
      Pins("A18", "B18"),
       Std("LVCMOS33")),

    "SERIAL" -> XdcInfo.multi(
      "tx" -> Seq(
        // Sch=UART_TXD_IN
        Pin("J17"), Std("LVCMOS33")),
      "rx" -> Seq(
        // Sch=UART_RXD_OUT
        Pin("J18"), Std("LVCMOS33"))),

    "FLASH" -> XdcInfo.multi(
      "ssel_n" -> Seq(
        // Sch=QSPI_CS
        Pin("K19"), Std("LVCMOS33")),
      "dq" -> Seq(
        // Sch=QSPI_DQ{0..3}
        Pins("D18", "D19", "G18", "F18"),
         Std("LVCMOS33"))),

    "CRYPTO_SDA" -> XdcInfo(
      // Sch=CRYPTO_SDA
      Pin("D17"), Std("LVCMOS33")),

    "PMODA" -> XdcInfo(
      // Sch=JA{1..4, 7..10}
      Pins("G17", "G19", "N18", "L18", "H17", "H19", "J19", "K18"),
       Std("LVCMOS33")),

    "XADC" -> XdcInfo.multi(
      "n" -> Seq(
        // Sch=AIN_N{15..16}
        Pins("G2", "J2"),
         Std("LVCMOS33")),
      "p" -> Seq(
        // Sch=AIN_P{15..16}
        Pins("G3", "H2"),
         Std("LVCMOS33"))),

    "GPIO" -> XdcInfo(
      // Sch=PIO{1..14},AIN_P{15,16},PIO{17..48}
      Pins("__", // 0
           "M3",  "L3",  "A16", "K3",  "C15", "H1",  "A15", "B15", // 1..8
           "A14", "J3",  "J1",  "K2",  "L1",  "L2",  "__",  "__",  // 9..16
           "M1",  "N3",  "P3",  "M2",  "N1",  "N2",  "P1",  "__",  // 17..24
           "__",  "R3",  "T3",  "R2",  "T1",  "T2",  "U1",  "W2",  // 25..32
           "V2",  "W3",  "V3",  "W5",  "V4",  "U4",  "V5",  "W4",  // 33..40
           "U5",  "U2",  "W6",  "U3",  "U7",  "W7",  "U8",  "V8"), // 41..48
       Std("LVCMOS33")),

    "RAM" -> XdcInfo.multi(
      "addr" -> Seq(
        // Sch=SRAM_A{0..18}
        Pins("M18", "M19", "K17", "N17", "P17", "P18", "R18", "W19",
             "U19", "V19", "W18", "T17", "T18", "U17", "U18", "V16",
             "W16", "W17", "V15"),
         Std("LVCMOS33")),
      "data" -> Seq(
        // Sch=SRAM_DQ{0..7}
        Pins("W15", "W13", "W14", "U15", "U16", "V13", "V14", "U14"),
         Std("LVCMOS33")),
      "oe_n" -> Seq(
        // Sch=SRAM_OE
        Pin("P19"), Std("LVCMOS33")),
      "we_n" -> Seq(
        // Sch=SRAM_WE
        Pin("R19"), Std("LVCMOS33")),
      "ce_n" -> Seq(
        // Sch=SRAM_CE
        Pin("N19"), Std("LVCMOS33"))))
}

