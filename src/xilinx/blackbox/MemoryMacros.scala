package hwlib.xilinx.blackbox

import spinal.core._


case class FifoMacroConfig(
    readBits          : Int,
    writeBits         : Int,
    writeDepth        : Int,
    useOverflow       : Boolean = false,
    useUnderflow      : Boolean = false,
    useWrCount        : Boolean = false,
    useRdCount        : Boolean = false,
    useAlmostFull     : Boolean = false,
    useAlmostEmpty    : Boolean = false,
    progFullAt        : Option[Int] = None,
    progEmptyAt       : Option[Int] = None,
    useWrAck          : Boolean = false,
    useRdValid        : Boolean = false,
    useSleep          : Boolean = false,
    useECC            : Boolean = false,
    preferDistributed : Boolean = false,
    readLatency       : Int = 0) {
  require(readBits > 0)
  require(readBits <= 4096)
  require(writeBits > 0)
  require(writeBits <= 4096)
  require(readLatency >= 0)
  require((readBits == writeBits) ||
          (readBits * 2 == writeBits) ||
          (readBits * 4 == writeBits) ||
          (readBits * 8 == writeBits) ||
          (readBits == writeBits * 2) ||
          (readBits == writeBits * 4) ||
          (readBits == writeBits * 8))
  require((writeDepth * writeBits) % readBits == 0)

  require(readLatency >= 0)
  val readMode = if (readLatency > 0) { "std" } else { "fwft" }

  val wakeupTime = if (useSleep) { 2 } else { 0 }

  val writeCountBits = log2Up(writeDepth) + 1
  val readCountBits = log2Up(writeDepth * writeBits / readBits) + 1

  def TWrData = Bits(writeBits bits)
  def TRdData = Bits(readBits bits)

  def TWrCount = UInt(writeCountBits bits)
  def TRdCount = UInt(readCountBits bits)

  var memoryType = "auto"
  if (useSleep) {
    memoryType = "ultra"
  }
  if ((readBits != writeBits || useECC) && memoryType != "ultra") {
    memoryType = "block"
  }

  if (memoryType == "auto" && preferDistributed) {
    memoryType = "distributed"
  }

  val eccMode = if (useECC) { "en_ecc" } else { "no_ecc" }

  val useProgEmpty = progEmptyAt.isDefined
  val useProgFull = progFullAt.isDefined
  var advFeatureBits = 0
  advFeatureBits |= (if (useOverflow)   0x0001 else 0) | (if (useUnderflow)   0x0100 else 0)
  advFeatureBits |= (if (useProgFull)   0x0002 else 0) | (if (useProgEmpty)   0x0200 else 0)
  advFeatureBits |= (if (useWrCount)    0x0004 else 0) | (if (useRdCount)     0x0400 else 0)
  advFeatureBits |= (if (useAlmostFull) 0x0008 else 0) | (if (useAlmostEmpty) 0x0800 else 0)
  advFeatureBits |= (if (useWrAck)      0x0010 else 0) | (if (useRdValid)     0x1000 else 0)
  val advFeatures = f"$advFeatureBits%04x"
}

class FifoMacro(val cfg : FifoMacroConfig, val writeClockDomain : ClockDomain = null, val readClockDomain : ClockDomain = null) extends BlackBox {
  val wrDomain = if (null != writeClockDomain) {
    writeClockDomain
  } else if (null != readClockDomain) {
    readClockDomain
  } else {
    ClockDomain.current
  }

  val rdDomain = if (null != readClockDomain) {
    readClockDomain
  } else if (null != writeClockDomain) {
    writeClockDomain
  } else {
    ClockDomain.current
  }

  val isAsync = rdDomain != wrDomain

  val macroName = if (isAsync) { "xpm_fifo_async" } else { "xpm_fifo_sync" }
  if (globalData.config.mode == VHDL) {
    librariesUsages += "xpm"
    setDefinitionName(s"vcomponents.${macroName}")
  } else {
    setDefinitionName(macroName)
  }

  // CDC_SYNC_STAGES (2..8) *2 !ASYNC ONLY
  // RELATED_CLOCKS (0, 1) *0 !ASYNC_ONLY // TODO-lw detect if clocks are related...
  // SIM_ASSERT_CHK (0, 1) *0
  // CASCADE_HEIGHT (0..64) *0
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
  if (cfg.progEmptyAt.isDefined) {
    addGeneric("PROG_EMPTY_THRESH", cfg.progEmptyAt.get)
  }
  // PROG_FULL_THRESH (5 .. 4194301) *10
  if (cfg.progFullAt.isDefined) {
    addGeneric("PROG_FULL_THRESH", cfg.progFullAt.get)
  }

  // READ_MODE ("std", "fwft") *"std" ("fwft" requires FIFO_READ_LATENCY = 0)
  addGeneric("READ_MODE", cfg.readMode)
  // FIFO_READ_LATENCY (0..10/100) *1
  addGeneric("FIFO_READ_LATENCY", cfg.readLatency)

  // WAKEUP_TIME (0, 2) *0 (disables sleep pin)
  addGeneric("WAKEUP_TIME", cfg.wakeupTime)

  // FIFO_WRITE_DEPTH (16..419304) *2048
  addGeneric("FIFO_WRITE_DEPTH", cfg.writeDepth)
  // WRITE_DATA_WIDTH (1 .. 4096) * 32
  addGeneric("WRITE_DATA_WIDTH", cfg.writeBits)
  // READ_DATA_WIDTH (1 .. 4096) *32
  addGeneric("READ_DATA_WIDTH", cfg.readBits)
  // RD_DATA_COUNT_WIDTH (1 .. 23) *1
  addGeneric("RD_DATA_COUNT_WIDTH", cfg.readCountBits)
  // WR_DATA_COUNT_WIDTH (1 .. 23) * 1
  addGeneric("WR_DATA_COUNT_WIDTH", cfg.writeCountBits)

  val io = new Bundle {
    val wr_clk = in(Bool)
    val rd_clk = isAsync generate in(Bool)

    val rst    = in(Bool) default False   // wr_clk

    val wr_rst_busy   = out(Bool)         // wr_clk
    val wr_en         =  in(Bool)         // wr_clk
    val din           =  in(cfg.TWrData)  // wr_clk
    val wr_data_count = out(cfg.TWrCount) // wr_clk
    val wr_ack        = out(Bool)         // wr_clk
    val almost_full   = out(Bool)         // wr_clk
    val full          = out(Bool)         // wr_clk
    val overflow      = out(Bool)         // wr_clk
    val prog_full     = out(Bool)         // wr_clk

    val rd_rst_busy   = out(Bool)         // rd_clk
    val rd_en         =  in(Bool)         // rd_clk
    val rd_data_count = out(cfg.TRdCount) // rd_clk
    val dout          = out(cfg.TRdData)  // rd_clk
    val data_valid    = out(Bool)         // rd_clk
    val almost_empty  = out(Bool)         // rd_clk
    val empty         = out(Bool)         // rd_clk
    val prog_empty    = out(Bool)         // rd_clk
    val underflow     = out(Bool)         // rd_clk

    val sleep = in(Bool) default False

    val injectdbiterr = in(Bool) default False // wr_clk
    val injectsbiterr = in(Bool) default False // wr_clk
    val dbiterr       = out(Bool)              // rd_clk
    val sbiterr       = out(Bool)              // rd_clk
  }

  mapClockDomain(wrDomain, io.wr_clk)
  if (isAsync) {
    mapClockDomain(rdDomain, io.rd_clk)
  }

  noIoPrefix()
  addTag(noNumericType)
}


class Fifo[T <: Data](tPayload : HardType[T], logDepth : Int) extends Component {
  require(logDepth > 0)

  val dataBits = widthOf(tPayload())
  val cfg = FifoMacroConfig(dataBits, dataBits, 1 << logDepth, readLatency = 0)

  val io = new Bundle {
    val iPutData  =  in(tPayload())
    val iPutValid =  in(Bool)
    val oPutReady = out(Bool)
    val oGetData  = out(tPayload())
    val oGetValid = out(Bool)
    val iGetReady =  in(Bool)

    /*
    val oCount    = out(UInt((logDepth + 1) bits))
    val oSpace    = out(UInt((logDepth + 1) bits))
    val oFull     = out(Bool)
    val oEmpty    = out(Bool)
    */
  }

  val iMacro = new FifoMacro(cfg)

  iMacro.io.din := io.iPutData.asBits
  iMacro.io.wr_en := io.iPutValid && !iMacro.io.full
  io.oPutReady := !iMacro.io.full

  io.oGetData.assignFromBits(iMacro.io.dout)
  io.oGetValid := !iMacro.io.empty
  iMacro.io.rd_en := io.iGetReady && !iMacro.io.empty

}
