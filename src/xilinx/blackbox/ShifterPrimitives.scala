package hwlib.xilinx.blackbox

import spinal.core._



class Shifter16(val init : BigInt = 0) extends BlackBox {
  val primitiveName = "SRL16E"
  if (globalData.config.mode == VHDL) {
    librariesUsages += "unisim"
    setDefinitionName(s"vcomponents.${primitiveName}")
  } else {
    setDefinitionName(primitiveName)
  }

  addGeneric("INIT", U(init, 16 bits))

  val io = new Bundle {
    val Q   = out(Bool)
    val A0  =  in(Bool)
    val A1  =  in(Bool)
    val A2  =  in(Bool)
    val A3  =  in(Bool)
    val CE  =  in(Bool)
    val D   =  in(Bool)
    val CLK =  in(Bool)
  }

  mapCurrentClockDomain(io.CLK)

  noIoPrefix()
  addTag(noNumericType)
}

class Shifter32(val init : BigInt = 0) extends BlackBox {
  val primitiveName = "SRLC32E"
  if (globalData.config.mode == VHDL) {
    librariesUsages += "unisim"
    setDefinitionName(s"vcomponents.${primitiveName}")
  } else {
    setDefinitionName(primitiveName)
  }

  addGeneric("INIT", U(init, 32 bits))

  val io = new Bundle {
    val Q   = out(Bool)
    val Q31 = out(Bool)
    val A   =  in(UInt(5 bits))
    val CE  =  in(Bool)
    val D   =  in(Bool)
    val CLK =  in(Bool)
  }

  mapCurrentClockDomain(io.CLK)

  noIoPrefix()
  addTag(noNumericType)
}

class Shifter(val size : Int, val dataBits : Int) extends Component {
  require(size > 1)
  require(size <= 128)
  require(dataBits > 0)

  val addrBits = log2Up(size)

  val countLarge = if (size <= 16) {
    0
  } else if (size <= 32) {
    1
  } else if (size <= 64) {
    2
  } else if (size <= 96) {
    3
  } else {
    4
  }

  val io = new Bundle {
    val iEnable =  in(Bool)
    val iData   =  in(Bits(dataBits bits))
    val iAddr   =  in(UInt(addrBits bits))
    val oData   = out(Bits(dataBits bits))
  }

  val iLanes = for (idx <- 0 until dataBits) yield new Area {

    val iChain = if (countLarge == 0) new Area {
      val iPrimitive = new Shifter16()
      iPrimitive.io.CE := io.iEnable
      iPrimitive.io.A0 := io.iAddr(0)
      iPrimitive.io.A1 := (if (addrBits >= 1) { io.iAddr(1) } else { False })
      iPrimitive.io.A2 := (if (addrBits >= 2) { io.iAddr(2) } else { False })
      iPrimitive.io.A3 := (if (addrBits >= 3) { io.iAddr(3) } else { False })
      iPrimitive.io.D := io.iData(idx)
      io.oData(idx) := iPrimitive.io.Q
    } else new Area {
      val iPrimitives = for (idxP <- 0 until countLarge) yield new Shifter32()
      for (idxP <- 0 until countLarge){
        iPrimitives(idxP).io.CE := io.iEnable
        iPrimitives(idxP).io.A  := io.iAddr(4 downto 0)
        if (idxP == 0) {
          iPrimitives(idxP).io.D := io.iData(idx)
        } else {
          iPrimitives(idxP).io.D := iPrimitives(idxP-1).io.Q31
        }
      }
      if (countLarge == 1) {
        io.oData(idx) := iPrimitives(0).io.Q
      } else if (countLarge == 2) {
        io.oData(idx) := io.iAddr(5).mux(
            0 -> iPrimitives(0).io.Q,
            1 -> iPrimitives(1).io.Q)
      } else if (countLarge >= 3) {
        io.oData(idx) := io.iAddr(6 downto 5).mux(
            0 -> iPrimitives(0).io.Q,
            1 -> iPrimitives(1).io.Q,
            2 -> iPrimitives(2).io.Q,
            3 -> (if (countLarge > 3) { iPrimitives(3).io.Q } else { False }))
      }
    }

  }

}

