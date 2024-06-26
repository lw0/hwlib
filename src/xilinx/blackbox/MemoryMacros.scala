package hwlib.xilinx.blackbox

import spinal.core._


/*
case class DistRamMacroConfig(
    putDepth          : Int,
    putBits           : Int,
    getBits           : Int,
    withPutOver       : Boolean = false,
    withGetUnder      : Boolean = false,
    withPutCount      : Boolean = false,
    withGetCount      : Boolean = false,
    withPutNearFull   : Boolean = false,
    withGetNearEmpty  : Boolean = false,
    putProgFullAt     : Option[Int] = None,
    getProgEmptyAt    : Option[Int] = None,
    withPutAck        : Boolean = false,
    withGetValid      : Boolean = false,
    withSleep         : Boolean = false,
    withError         : Boolean = false,
    preferDistributed : Boolean = false,
    simAssert         : Boolean = false,
    related           : Boolean = false,
    syncStages        : Int = 2,
    readLatency       : Int = 0) { // 0 enables fwft
  require(getBits > 0)
  require(getBits <= 4096)
  require(putBits > 0)
  require(putBits <= 4096)
  require(readLatency >= 0)
  require((getBits == putBits) ||
          (getBits * 2 == putBits) ||
          (getBits * 4 == putBits) ||
          (getBits * 8 == putBits) ||
          (getBits == putBits * 2) ||
          (getBits == putBits * 4) ||
          (getBits == putBits * 8))
  require((putDepth * putBits) % getBits == 0)
  require(syncStages >= 2 && syncStages <= 8)

  require(readLatency >= 0)
  val readMode = if (readLatency > 0) { "std" } else { "fwft" }

  val wakeupTime = if (withSleep) { 2 } else { 0 }

  val writeCountBits = log2Up(putDepth) + 1
  val readCountBits = log2Up(putDepth * putBits / getBits) + 1

  def TWrData = Bits(putBits bits)
  def TRdData = Bits(getBits bits)

  def TWrCount = UInt(writeCountBits bits)
  def TRdCount = UInt(readCountBits bits)

  var memoryType = "auto"
  if (withSleep) {
    memoryType = "ultra"
  }
  if ((getBits != putBits || withError) && memoryType != "ultra") {
    memoryType = "block"
  }

  if (memoryType == "auto" && preferDistributed) {
    memoryType = "distributed"
  }

  val eccMode = if (withError) { "en_ecc" } else { "no_ecc" }

  val useProgEmpty = getProgEmptyAt.isDefined
  val useProgFull = putProgFullAt.isDefined
  var advFeatureBits = 0
  advFeatureBits |= (if (withPutOver)   0x0001 else 0) | (if (withGetUnder)   0x0100 else 0)
  advFeatureBits |= (if (useProgFull)   0x0002 else 0) | (if (useProgEmpty)   0x0200 else 0)
  advFeatureBits |= (if (withPutCount)    0x0004 else 0) | (if (withGetCount)     0x0400 else 0)
  advFeatureBits |= (if (withPutNearFull) 0x0008 else 0) | (if (withGetNearEmpty) 0x0800 else 0)
  advFeatureBits |= (if (withPutAck)      0x0010 else 0) | (if (withGetValid)     0x1000 else 0)
  val advFeatures = f"$advFeatureBits%04x"
}

class DistRamMacro(val cfg : DistRamMacroConfig) extends BlackBox {

  val macroName = "xpm_memory_dpdistram"
  if (globalData.config.mode == VHDL) {
    librariesUsages += "xpm"
    setDefinitionName(s"vcomponents.${macroName}")
  } else {
    setDefinitionName(macroName)
  }


  // DOUT_RESET_VALUE Bits
  // FULL_RESET_VALUE Bool

  // USE_ADV_FEATURES (12 bit hex string) *"0707"
  /*
    Bit 0  enables overflow flag     *1
    Bit 1  enables prog_full flag    *1
    Bit 2  enables wr_data_count     *1
    Bit 3  enables almost_full flag  *0
    Bit 4  enables wr_ack flag       *0
    Bit 8  enables underflow flag    *1
    Bit 9  enables prog_empty flag   *1
    Bit 10 enables rd_data_count     *1
    Bit 11 enables almost_empty flag *0
    Bit 12 enables data_valid flag   *0
   */
  addGeneric("USE_ADV_FEATURES", cfg.advFeatures)

  // FIFO_MEMORY_TYPE("auto", "block", "distributed") *"auto"
  addGeneric("FIFO_MEMORY_TYPE", cfg.memoryType)
  // ECC_MODE ("no_ecc", "en_ecc") *"no_ecc"
  addGeneric("ECC_MODE", cfg.eccMode)

  // PROG_EMPTY_THRESH (3 .. 4194301) *10
  if (cfg.getProgEmptyAt.isDefined) {
    addGeneric("PROG_EMPTY_THRESH", cfg.getProgEmptyAt.get)
  }
  // PROG_FULL_THRESH (5 .. 4194301) *10
  if (cfg.putProgFullAt.isDefined) {
    addGeneric("PROG_FULL_THRESH", cfg.putProgFullAt.get)
  }

  // READ_MODE ("std", "fwft") *"std" ("fwft" requires FIFO_READ_LATENCY = 0)
  addGeneric("READ_MODE", cfg.readMode)
  // FIFO_READ_LATENCY (0..10/100) *1
  addGeneric("FIFO_READ_LATENCY", cfg.readLatency)

  // WAKEUP_TIME (0, 2) *0 (disables sleep pin)
  addGeneric("WAKEUP_TIME", cfg.wakeupTime)

  // FIFO_WRITE_DEPTH (16..419304) *2048
  addGeneric("FIFO_WRITE_DEPTH", cfg.putDepth)
  // WRITE_DATA_WIDTH (1 .. 4096) * 32
  addGeneric("WRITE_DATA_WIDTH", cfg.putBits)
  // READ_DATA_WIDTH (1 .. 4096) *32
  addGeneric("READ_DATA_WIDTH", cfg.getBits)
  // RD_DATA_COUNT_WIDTH (1 .. 23) *1
  addGeneric("RD_DATA_COUNT_WIDTH", cfg.readCountBits)
  // WR_DATA_COUNT_WIDTH (1 .. 23) * 1
  addGeneric("WR_DATA_COUNT_WIDTH", cfg.writeCountBits)

  /////////////////////

  addGeneric("CLOCKING_MODE", "common_clocks");

  def TAAddr = Bits(addrBitsA bits)
  def TBAddr = Bits(addrBitsB bits)

  def TAData = Bits(dataBitsA bits)
  def TBData = Bits(dataBitsB bits)

  val io = new Bundle {
    val clk = in(Bool)
    val rst = in(Bool)

    val iAAddr =  in(TAAddr)
    val iAData =  in(TAData)
    val iAWrEn =  in(Bool)
    val oAData = out(TAData)

    val iBAddr =  in(TBAddr)
    val oBData = out(TBData)

    val oPutInReset   = out(Bool)         // wr_clk
    val iPutData      =  in(cfg.TWrData)  // wr_clk
    val iPutEnable    =  in(Bool)         // wr_clk
    val oPutFull      = out(Bool)         // wr_clk
    val oPutNearFull  = out(Bool)         // wr_clk
    val oPutProgFull  = out(Bool)         // wr_clk
    val oPutOver      = out(Bool)         // wr_clk
    val oPutAck       = out(Bool)         // wr_clk
    val oPutCount     = out(cfg.TWrCount) // wr_clk

    val oGetInReset   = out(Bool)         // rd_clk
    val oGetData      = out(cfg.TRdData)  // rd_clk
    val iGetEnable    =  in(Bool)         // rd_clk
    val oGetEmpty     = out(Bool)         // rd_clk
    val oGetNearEmpty = out(Bool)         // rd_clk
    val oGetProgEmpty = out(Bool)         // rd_clk
    val oGetUnder     = out(Bool)         // rd_clk
    val oGetValid     = out(Bool)         // rd_clk
    val oGetCount     = out(cfg.TRdCount) // rd_clk

    val iSleep = in(Bool) default False

    val iErrDoubleInject = in(Bool) default False // wr_clk
    val iErrSingleInject = in(Bool) default False // wr_clk
    val oErrDouble       = out(Bool)              // rd_clk
    val oErrSingle       = out(Bool)              // rd_clk
  }

  io.oPutInReset.setName("wr_rst_busy")
  io.iPutData.setName("din")
  io.iPutEnable.setName("wr_en")
  io.oPutFull.setName("full")
  io.oPutNearFull.setName("almost_full")
  io.oPutProgFull.setName("prog_full")
  io.oPutOver.setName("overflow")
  io.oPutAck.setName("wr_ack")
  io.oPutCount.setName("wr_data_count")
  io.oGetInReset.setName("rd_rst_busy")
  io.oGetData.setName("dout")
  io.iGetEnable.setName("rd_en")
  io.oGetEmpty.setName("empty")
  io.oGetNearEmpty.setName("almost_empty")
  io.oGetProgEmpty.setName("prog_empty")
  io.oGetUnder.setName("underflow")
  io.oGetValid.setName("data_valid")
  io.oGetCount.setName("rd_data_count")
  io.iSleep.setName("sleep")
  io.iErrDoubleInject.setName("injectdbiterr")
  io.iErrSingleInject.setName("injectsbiterr")
  io.oErrDouble.setName("dbiterr")
  io.oErrSingle.setName("sbiterr")

  mapClockDomain(wrDomain, io.wr_clk, io.rst)
  if (isAsync) {
    mapClockDomain(rdDomain, io.rd_clk)
  }

  noIoPrefix()
  addTag(noNumericType)
}
*/
