package hwlib.amba

import scala.collection.mutable

import spinal.core._
import spinal.lib.Counter
import spinal.lib.fsm.{StateMachine, State, StateParallelFsm, EntryPoint}

import hwlib.bundle._
import hwlib.xilinx.tools.XilinxInterfaceType



case class AxiConfig(
  addrBits  : Int,
  dataBytes : Int,
  hasStrb   : Boolean = true,
  hasBurst  : Boolean = true,
  hasLock   : Boolean = false,
  hasCache  : Boolean = false,
  hasProt   : Boolean = false,
  hasQos    : Boolean = false,
  hasRegion : Boolean = false,
  hasRd     : Boolean = true,
  hasWr     : Boolean = true,
  isAxi3    : Boolean = false,
  wrInterleaveDepth : Int = 1,
  idBits    : Int = 0,
  aUserBits : Int = 0,
  wUserBits : Int = 0,
  bUserBits : Int = 0,
  rUserBits : Int = 0) {
  require(Seq(1, 2, 4, 8, 16, 32, 64, 128).contains(dataBytes))
  val fullSize  = log2Up(dataBytes)
  require(addrBits >= fullSize && addrBits <= 64)
  require(wrInterleaveDepth > 0)
  require(idBits >= 0 && idBits <= 64)
  require(aUserBits >= 0)
  require(wUserBits >= 0)
  require(bUserBits >= 0)
  require(rUserBits >= 0)

  val maxLogVolume = if (hasBurst) {
    if (isAxi3) {
      // maximum 16 INCR beats at fullSize, but no more than to cross the 4K / 12bit boundary
      (fullSize + 4) min 12
    } else {
      // maximum 256 INCR beats at fullSize, but no more than to cross the 4K / 12bit boundary
      (fullSize + 8) min 12
    }
  } else {
    fullSize
  }

  // Address
  val bound = 12 min addrBits
  val waddrBits = addrBits - fullSize
  val wBound    = bound - fullSize
  def TAddr = UInt(addrBits bits)
  def TWIdx = UInt(fullSize bits)
  def TWAddr = UInt(waddrBits bits)

  // Data
  val dataBits = dataBytes * 8
  def TData = Bits(dataBits bits)
  def TMask = Bits(dataBytes bits)
  def cMaskNone = TMask.getZero
  def cMaskAll = ~cMaskNone

  // Id
  val hasId = (idBits > 0)
  val hasWId = hasId && isAxi3
  val maxId = (1 << idBits) - 1
  def TId = UInt(idBits bits)

  // User
  val hasAUser = (aUserBits > 0)
  val hasWUser = (wUserBits > 0)
  val hasBUser = (bUserBits > 0)
  val hasRUser = (rUserBits > 0)
  def TAUser = Bits(aUserBits bits)
  def TWUser = Bits(wUserBits bits)
  def TBUser = Bits(bUserBits bits)
  def TRUser = Bits(rUserBits bits)

  // Resp
  def TResp = Bits(2 bits)
  def cRespOkay   = B(0, widthOf(TResp) bits)
  def cRespExOkay = B(1, widthOf(TResp) bits)
  def cRespSlvErr = B(2, widthOf(TResp) bits)
  def cRespDecErr = B(3, widthOf(TResp) bits)

  // Burst
  val lenBits = if (isAxi3) { 4 } else { 8 }
  def TLen = UInt(lenBits bits)
  def cLenSingle = U(0, widthOf(TLen) bits)

  def TSize = UInt(3 bits)
  def cSizeFull = U(fullSize, widthOf(TSize) bits)

  def TBurst = Bits(2 bits)
  def cBurstFixed = B(0, widthOf(TBurst) bits)
  def cBurstIncr  = B(1, widthOf(TBurst) bits)
  def cBurstWrap  = B(2, widthOf(TBurst) bits)

  // Attributes
  val lockBits = if (isAxi3) { 2 } else { 1 }
  def TLock = Bits(lockBits bits)
  def cLockNormal = B(0, widthOf(TLock) bits)
  def cLockExcl   = B(1, widthOf(TLock) bits)
  def cLockLocked = B(2, widthOf(TLock) bits)

  def TCache = Bits(4 bits)
  def cCacheNone      = B(0x0, widthOf(TCache) bits)
  def cCacheBufferBit = B(0x1, widthOf(TCache) bits)
  def cCacheCacheBit  = B(0x2, widthOf(TCache) bits)
  def cCacheRAllocBit = B(0x4, widthOf(TCache) bits)
  def cCacheWAllocBit = B(0x8, widthOf(TCache) bits)
  def cCacheDevice    = B(0x0, widthOf(TCache) bits)
  def cCacheDeviceBuf = B(0x1, widthOf(TCache) bits)
  def cCacheNormal    = B(0x2, widthOf(TCache) bits)
  def cCacheNormalBuf = B(0x3, widthOf(TCache) bits)
  def cCacheThroughNoAlloc_AW   = B(0x6, widthOf(TCache) bits)
  def cCacheThroughRdAlloc_AW   = B(0x6, widthOf(TCache) bits)
  def cCacheThroughWrAlloc_AW   = B(0xe, widthOf(TCache) bits)
  def cCacheThroughRdWrAlloc_AW = B(0xe, widthOf(TCache) bits)
  def cCacheBackNoAlloc_AW      = B(0x7, widthOf(TCache) bits)
  def cCacheBackRdAlloc_AW      = B(0x7, widthOf(TCache) bits)
  def cCacheBackWrAlloc_AW      = B(0xf, widthOf(TCache) bits)
  def cCacheBackRdWrAlloc_AW    = B(0xf, widthOf(TCache) bits)
  def cCacheThroughNoAlloc_AR   = B(0xa, widthOf(TCache) bits)
  def cCacheThroughRdAlloc_AR   = B(0xe, widthOf(TCache) bits)
  def cCacheThroughWrAlloc_AR   = B(0xa, widthOf(TCache) bits)
  def cCacheThroughRdWrAlloc_AR = B(0xe, widthOf(TCache) bits)
  def cCacheBackNoAlloc_AR      = B(0xb, widthOf(TCache) bits)
  def cCacheBackRdAlloc_AR      = B(0xf, widthOf(TCache) bits)
  def cCacheBackWrAlloc_AR      = B(0xb, widthOf(TCache) bits)
  def cCacheBackRdWrAlloc_AR    = B(0xf, widthOf(TCache) bits)

  def TProt = Bits(3 bits)
  def cProtNone    = B(0x0, widthOf(TProt) bits)
  def cProtPrivBit = B(0x1, widthOf(TProt) bits)
  def cProtNSecBit = B(0x2, widthOf(TProt) bits)
  def cProtInstBit = B(0x4, widthOf(TProt) bits)

  def TQos = Bits(4 bits)
  def cQosNone = B(0, widthOf(TQos) bits)

  def TRegion = Bits(4 bits)
  def cRegionNone = B(0, widthOf(TRegion) bits)

  val isLite = !hasBurst && !hasAUser && !hasWUser && !hasBUser && !hasRUser
}

class AxiDefaults(defStrb   : () => Bits,
                  defLast   : () => Bool,
                  defLen    : () => UInt,
                  defSize   : () => Bits,
                  defBurst  : () => Bits,
                  defId     : () => UInt,
                  defLock   : () => Bits,
                  defCache  : () => Bits,
                  defProt   : () => Bits,
                  defQos    : () => Bits,
                  defRegion : () => Bits,
                  defAUser  : () => Bits,
                  defWUser  : () => Bits,
                  defBUser  : () => Bits,
                  defRUser  : () => Bits) {

  def getStrb(cfg : AxiConfig) = ifGen(cfg.hasStrb) {
    val d = defStrb()
    if (d != null) { d } else { cfg.cMaskAll }.resize(widthOf(cfg.TMask) bits)
  }
  def getLast(cfg : AxiConfig) = ifGen(cfg.hasBurst) {
    val d = defLast()
    if (d != null) { d } else { True }
  }
  def getLen(cfg : AxiConfig) = ifGen(cfg.hasBurst)  {
    val d = defLen()
    if (d != null) { d } else { cfg.cLenSingle }.resize(widthOf(cfg.TLen) bits)
  }
  def getSize(cfg : AxiConfig) = ifGen(cfg.hasBurst)  {
    val d = defSize()
    if (d != null) { d } else { cfg.cSizeFull }.resize(widthOf(cfg.TSize) bits)
  }
  def getBurst(cfg : AxiConfig) = ifGen(cfg.hasBurst)  {
    val d = defBurst()
    if (d != null) { d } else { cfg.cBurstFixed }.resize(widthOf(cfg.TBurst) bits)
  }
  def getId(cfg : AxiConfig) = ifGen(cfg.hasId) {
    val d = defId()
    if (d != null) { d } else { U(0) }.resize(widthOf(cfg.TId) bits)
  }
  def getLock(cfg : AxiConfig) = ifGen(cfg.hasLock)   {
    val d = defLock()
    if (d != null) { d } else { cfg.cLockNormal }.resize(widthOf(cfg.TLock) bits)
  }
  def getCache(cfg : AxiConfig) = ifGen(cfg.hasCache)  {
    val d = defCache()
    if (d != null) { d } else { cfg.cCacheNormal }.resize(widthOf(cfg.TCache) bits)
  }
  def getProt(cfg : AxiConfig) = ifGen(cfg.hasProt)   {
    val d = defProt()
    if (d != null) { d } else { cfg.cProtNone }.resize(widthOf(cfg.TProt) bits)
  }
  def getQos(cfg : AxiConfig) = ifGen(cfg.hasQos)    {
    val d = defQos()
    if (d != null) { d } else { cfg.cQosNone }.resize(widthOf(cfg.TQos) bits)
  }
  def getRegion(cfg : AxiConfig) = ifGen(cfg.hasRegion) {
    val d = defRegion()
    if (d != null) { d } else { cfg.cRegionNone }.resize(widthOf(cfg.TRegion) bits)
  }
  def getAUser(cfg : AxiConfig) = ifGen(cfg.hasAUser) {
    val d = defAUser()
    if (d != null) { d } else { B(0) }.resize(widthOf(cfg.TAUser) bits)
  }
  def getWUser(cfg : AxiConfig) = ifGen(cfg.hasWUser) {
    val d = defWUser()
    if (d != null) { d } else { B(0) }.resize(widthOf(cfg.TWUser) bits)
  }
  def getBUser(cfg : AxiConfig) = ifGen(cfg.hasBUser) {
    val d = defBUser()
    if (d != null) { d } else { B(0) }.resize(widthOf(cfg.TBUser) bits)
  }
  def getRUser(cfg : AxiConfig) = ifGen(cfg.hasRUser) {
    val d = defRUser()
    if (d != null) { d } else { B(0) }.resize(widthOf(cfg.TRUser) bits)
  }
}

object AxiDefaults {
  def apply(
        defStrb   : => Bits = null,
        defLast   : => Bool = null,
        defLen    : => UInt = null,
        defSize   : => Bits = null,
        defBurst  : => Bits = null,
        defId     : => UInt = null,
        defLock   : => Bits = null,
        defCache  : => Bits = null,
        defProt   : => Bits = null,
        defQos    : => Bits = null,
        defRegion : => Bits = null,
        defAUser  : => Bits = null,
        defWUser  : => Bits = null,
        defBUser  : => Bits = null,
        defRUser  : => Bits = null) =
      new AxiDefaults(
        () => defStrb,
        () => defLast,
        () => defLen,
        () => defSize,
        () => defBurst,
        () => defId,
        () => defLock,
        () => defCache,
        () => defProt,
        () => defQos,
        () => defRegion,
        () => defAUser,
        () => defWUser,
        () => defBUser,
        () => defRUser)
}

case class TAxiAux(val cfg : AxiConfig, val defaults : AxiDefaults = AxiDefaults()) extends AutoBundle {
  val lock   = ifGen(cfg.hasLock)   { cfg.TLock }
  val cache  = ifGen(cfg.hasCache)  { cfg.TCache }
  val prot   = ifGen(cfg.hasProt)   { cfg.TProt }
  val qos    = ifGen(cfg.hasQos)    { cfg.TQos }
  val region = ifGen(cfg.hasRegion) { cfg.TRegion }
  val user   = ifGen(cfg.hasAUser)  { cfg.TAUser }

  element("lock",   WarnDefault(defaults.getLock(cfg)), WarnDrop)
  element("cache",  WarnDefault(defaults.getCache(cfg)), WarnDrop)
  element("prot",   WarnDefault(defaults.getProt(cfg)), WarnDrop)
  element("qos",    WarnDefault(defaults.getQos(cfg)), WarnDrop)
  element("region", WarnDefault(defaults.getRegion(cfg)), WarnDrop)
  element("user",   WarnDefault(defaults.getAUser(cfg)), WarnDrop, MayUpsize, WarnDownsize)
}

case class TAxiAPayload(val cfg : AxiConfig, val defaults : AxiDefaults = AxiDefaults()) extends AutoBundle {
  val addr   = cfg.TAddr
  val len    = ifGen(cfg.hasBurst)  { cfg.TLen }
  val size   = ifGen(cfg.hasBurst)  { cfg.TSize }
  val burst  = ifGen(cfg.hasBurst)  { cfg.TBurst }
  val id     = ifGen(cfg.hasId)     { cfg.TId }
  val aux    = TAxiAux(cfg, defaults)

  def waddr  = U(addr.takeHigh(widthOf(cfg.TWAddr)))
  def widx   = U(addr.takeLow(widthOf(cfg.TWIdx)))
  def lock   = aux.lock
  def cache  = aux.cache
  def prot   = aux.prot
  def qos    = aux.qos
  def region = aux.region
  def user   = aux.user

  element("addr",   MayUpsize, WarnDownsize)
  element("len",    WarnDefault(defaults.getLen(cfg)), WarnDrop)
  element("size",   WarnDefault(defaults.getSize(cfg)), WarnDrop)
  element("burst",  WarnDefault(defaults.getBurst(cfg)), WarnDrop)
  element("id",     WarnDefault(defaults.getId(cfg)), WarnDrop, MayUpsize, WarnDownsize)
  element("aux")
}

case class TAxiA(val cfg : AxiConfig, val defaults : AxiDefaults = AxiDefaults()) extends Channel(TAxiAPayload(cfg, defaults)) {
  def addr   = payload.addr
  def waddr  = payload.waddr
  def widx   = payload.widx
  def len    = payload.len
  def size   = payload.size
  def burst  = payload.burst
  def lock   = payload.aux.lock
  def cache  = payload.aux.cache
  def prot   = payload.aux.prot
  def qos    = payload.aux.qos
  def region = payload.aux.region
  def id     = payload.id
  def user   = payload.aux.user
}


case class TAxiWPayload(val cfg : AxiConfig, val defaults : AxiDefaults = AxiDefaults()) extends AutoBundle {
  val data = cfg.TData
  val strb = ifGen(cfg.hasStrb)  { cfg.TMask }
  val user = ifGen(cfg.hasWUser) { cfg.TWUser }
  val last = ifGen(cfg.hasBurst) { Bool }
  val id   = ifGen(cfg.hasWId)   { cfg.TId }

  element("data")
  element("strb", WarnDefault(defaults.getStrb(cfg)), WarnDrop)
  element("user", WarnDefault(defaults.getWUser(cfg)), WarnDrop, MayUpsize, WarnDownsize)
  element("last", WarnDefault(defaults.getLast(cfg)), WarnDrop)
  element("id",   WarnDefault(defaults.getId(cfg)), WarnDrop, MayUpsize, WarnDownsize)
}

case class TAxiW(val cfg : AxiConfig, val defaults : AxiDefaults = AxiDefaults()) extends Channel(TAxiWPayload(cfg, defaults)) {
  def data = payload.data
  def strb = payload.strb
  def user = payload.user
  def last = payload.last
  def id   = payload.id
}


case class TAxiBPayload(val cfg : AxiConfig, val defaults : AxiDefaults = AxiDefaults()) extends AutoBundle {
  val resp = cfg.TResp
  val id   = ifGen(cfg.hasId)    { cfg.TId }
  val user = ifGen(cfg.hasBUser) { cfg.TBUser }

  element("resp")
  element("id",   WarnDefault(defaults.getId(cfg)), MayUpsize, WarnDownsize, WarnDrop)
  element("user", WarnDefault(defaults.getBUser(cfg)), MayUpsize, WarnDownsize, WarnDrop)
}

case class TAxiB(val cfg : AxiConfig, val defaults : AxiDefaults = AxiDefaults()) extends Channel(TAxiBPayload(cfg, defaults)) {
  def resp = payload.resp
  def id   = payload.id
  def user = payload.user
}


case class TAxiRPayload(val cfg : AxiConfig, val defaults : AxiDefaults = AxiDefaults()) extends AutoBundle {
  val data = cfg.TData
  val resp = cfg.TResp
  val id   = ifGen(cfg.hasId)    { cfg.TId }
  val user = ifGen(cfg.hasRUser) { cfg.TRUser }
  val last = ifGen(cfg.hasBurst) { Bool }

  element("data")
  element("resp")
  element("id",   WarnDefault(defaults.getId(cfg)), WarnDrop, MayUpsize, WarnDownsize)
  element("user", WarnDefault(defaults.getRUser(cfg)), WarnDrop, MayUpsize, WarnDownsize)
  element("last", WarnDefault(defaults.getLast(cfg)), WarnDrop)
}

case class TAxiR(val cfg : AxiConfig, val defaults : AxiDefaults = AxiDefaults()) extends Channel(TAxiRPayload(cfg, defaults)) {
  def data = payload.data
  def resp = payload.resp
  def id   = payload.id
  def user = payload.user
  def last = payload.last
}


case class TAxi(val cfg : AxiConfig, val defaults : AxiDefaults = AxiDefaults()) extends AutoBundleBidir with XilinxInterfaceType{
  val aw = ifGen(cfg.hasWr) { TAxiA(cfg, defaults) }
  val w  = ifGen(cfg.hasWr) { TAxiW(cfg, defaults) }
  val b  = ifGen(cfg.hasWr) { TAxiB(cfg, defaults) }
  val ar = ifGen(cfg.hasRd) { TAxiA(cfg, defaults) }
  val r  = ifGen(cfg.hasRd) { TAxiR(cfg, defaults) }

  element("aw", false, UsePrefix, MayIgnore, MayDrop)
  element("w",  false, UsePrefix, MayIgnore, MayDrop)
  element("b",  true,  UsePrefix, MayIgnore, MayDrop)
  element("ar", false, UsePrefix, MayIgnore, MayDrop)
  element("r",  true,  UsePrefix, MayIgnore, MayDrop)

  def aReceiveFsm() = new StateMachine {
    val rPayload = TAxiAPayload(cfg).asRegInit()
    val rWasWrite = Reg(Bool()).init(False)
    aw.ready := False
    ar.ready := False

    val Wait = new State with EntryPoint {
      whenIsActive {
        when (aw.valid && (!rWasWrite || !ar.valid)) {
          rPayload := aw.payload
          rWasWrite := True
          goto(AckWrite)
        } elsewhen (ar.valid && (rWasWrite || !aw.valid)) {
          rPayload := ar.payload
          rWasWrite := False
          goto(AckRead)
        }
      }
    }

    val AckWrite : State = new State {
      whenIsActive {
        aw.ready := True
        ar.ready := False
        exit()
      }
    }

    val AckRead : State = new State {
      whenIsActive {
        aw.ready := False
        ar.ready := True
        exit()
      }
    }

  }

  def wrPlugFsm() = new StateMachine {
    val rId = ifGen(cfg.hasId) { Reg(cfg.TId) }
    aw.ready := False
    w.ready := False
    b.payload.assign("resp" -> cfg.cRespSlvErr, "id" -> rId)
    b.valid := False

    val RecvAW : State = new State with EntryPoint {
      whenIsActive {
        aw.ready := True
        when (aw.valid) {
          if (cfg.hasId) {
            rId := aw.id
          }
          goto(RecvW)
        }
      }
    }

    val RecvW : State = new State {
      whenIsActive {
        w.ready := True
        when (w.valid && (if (cfg.hasBurst) w.last else True)) {
          goto(SendB)
        }
      }
    }

    val SendB : State = new State {
      whenIsActive {
        b.valid := True
        when (b.ready) {
          goto(RecvAW)
        }
      }
    }
  }

  def rdPlugFsm() = new StateMachine {
    val rId = ifGen(cfg.hasId) { Reg(cfg.TId) }
    val rLen = ifGen(cfg.hasBurst) { Reg(cfg.TLen) }
    val rCount = ifGen(cfg.hasBurst) { Counter(widthOf(cfg.TLen) bits) }
    ar.ready := False
    r.payload.assign("resp" -> cfg.cRespSlvErr, "id" -> rId, "last" -> ifGen(cfg.hasBurst) { (rCount === rLen) })
    r.valid := False

    val RecvAR : State = new State with EntryPoint {
      whenIsActive {
        ar.ready := True
        when (ar.valid) {
          if (cfg.hasId) {
            rId := ar.id
          }
          if (cfg.hasBurst) {
            rLen := ar.len
            rCount.clear()
          }
          goto(SendR)
        }
      }
    }

    val SendR : State = new State {
      whenIsActive {
        r.valid := True
        when (r.ready) {
          if (cfg.hasBurst) {
            rCount.increment()
            when (rCount === rLen) {
              goto(RecvAR)
            } otherwise {
              rCount.increment()
            }
          } else {
            goto(RecvAR)
          }
        }
      }
    }
  }

  def plugFsm() = if (cfg.hasRd && cfg.hasWr) {
    new StateMachine {
      val PlugBoth : State = new StateParallelFsm(wrPlugFsm(), rdPlugFsm()) with EntryPoint
    }
  } else if (cfg.hasWr) {
    wrPlugFsm()
  } else if (cfg.hasRd) {
    rdPlugFsm()
  } else {
    null
  }

  def defineXilinxInterface() : String = {
    val interface = getName()
    val props = mutable.ArrayBuffer[String]()
    if (cfg.isAxi3) {
      if (cfg.isLite) {
        SpinalWarning("Axi3 Lite is not supported by xilinx interface, fallback to inference!")
      } else {
        props += s"PROTOCOL AXI3"
      }
    } else {
      if (cfg.isLite) {
        props += s"PROTOCOL AXI4LITE"
      } else {
        props += s"PROTOCOL AXI4"
      }
    }
    if (cfg.hasRd && cfg.hasWr) {
      props += s"READ_WRITE_MODE READ_WRITE"
    } else if (cfg.hasRd) {
      props += s"READ_WRITE_MODE READ_ONLY"
    } else if (cfg.hasWr) {
      props += s"READ_WRITE_MODE WRITE_ONLY"
    } else {
      SpinalWarning("Neither read or write AXI not supported by xilinx interface, but no signals anyway")
    }
    props += s"DATA_WIDTH ${cfg.dataBits}"
    props += s"ADDR_WIDTH ${cfg.addrBits}"
    props += s"ID_WIDTH ${cfg.idBits}"
    props += s"HAS_BURST ${if(cfg.hasBurst){"1"}else{"0"}}"
    props += s"HAS_CACHE ${if(cfg.hasCache){"1"}else{"0"}}"
    props += s"HAS_LOCK ${if(cfg.hasLock){"1"}else{"0"}}"
    props += s"HAS_PROT ${if(cfg.hasProt){"1"}else{"0"}}"
    props += s"HAS_QOS ${if(cfg.hasQos){"1"}else{"0"}}"
    props += s"HAS_REGION ${if(cfg.hasRegion){"1"}else{"0"}}"
    props += s"ARUSER_WIDTH ${cfg.aUserBits}"
    props += s"AWUSER_WIDTH ${cfg.aUserBits}"
    if (cfg.hasWr) {
      props += s"HAS_WSTRB ${if(cfg.hasStrb){"1"}else{"0"}}"
      props += s"HAS_BRESP 1"
      props += s"WUSER_WIDTH ${cfg.wUserBits}"
      props += s"BUSER_WIDTH ${cfg.bUserBits}"
    }
    if (cfg.hasRd) {
      props += s"HAS_RRESP 1"
      props += s"RUSER_WIDTH ${cfg.rUserBits}"
    }

    if (cfg.hasWr) {
      aw.addr.addAttribute(new AttributeString("X_INTERFACE_PARAMETER",
            props.mkString(", ")))
      aw.addr.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:interface:aximm:1.0 ${interface} AWADDR"))
      if (cfg.hasBurst) {
        aw.len.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} AWLEN"))
        aw.size.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} AWSIZE"))
        aw.burst.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} AWBURST"))
      }
      if (cfg.hasId) {
        aw.id.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} AWID"))
      }
      if (cfg.hasLock) {
        aw.lock.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} AWLOCK"))
      }
      if (cfg.hasCache) {
        aw.cache.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} AWCACHE"))
      }
      if (cfg.hasProt) {
        aw.prot.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} AWPROT"))
      }
      if (cfg.hasQos) {
        aw.qos.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} AWQOS"))
      }
      if (cfg.hasRegion) {
        aw.region.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} AWREGION"))
      }
      if (cfg.hasAUser) {
        aw.user.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} AWUSER"))
      }
      aw.valid.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:interface:aximm:1.0 ${interface} AWVALID"))
      aw.ready.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:interface:aximm:1.0 ${interface} AWREADY"))

      w.data.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:interface:aximm:1.0 ${interface} WDATA"))
      if (cfg.hasStrb) {
        w.strb.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} WSTRB"))
      }
      if (cfg.hasWUser) {
        w.user.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} WUSER"))
      }
      if (cfg.hasBurst) {
        w.last.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} WLAST"))
      }
      if (cfg.hasWId) {
        w.id.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} WID"))
      }
      w.valid.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:interface:aximm:1.0 ${interface} WVALID"))
      w.ready.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:interface:aximm:1.0 ${interface} WREADY"))

      b.resp.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:interface:aximm:1.0 ${interface} BRESP"))
      if (cfg.hasId) {
        b.id.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} BID"))
      }
      if (cfg.hasBUser) {
        b.user.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} BUSER"))
      }
      b.valid.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:interface:aximm:1.0 ${interface} BVALID"))
      b.ready.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:interface:aximm:1.0 ${interface} BREADY"))
    }
    if (cfg.hasRd) {
      if (!cfg.hasWr) {
        ar.addr.addAttribute(new AttributeString("X_INTERFACE_PARAMETER",
              props.mkString(", ")))
      }
      ar.addr.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:interface:aximm:1.0 ${interface} ARADDR"))
      if (cfg.hasBurst) {
        ar.len.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} ARLEN"))
        ar.size.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} ARSIZE"))
        ar.burst.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} ARBURST"))
      }
      if (cfg.hasId) {
        ar.id.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} ARID"))
      }
      if (cfg.hasLock) {
        ar.lock.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} ARLOCK"))
      }
      if (cfg.hasCache) {
        ar.cache.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} ARCACHE"))
      }
      if (cfg.hasProt) {
        ar.prot.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} ARPROT"))
      }
      if (cfg.hasQos) {
        ar.qos.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} ARQOS"))
      }
      if (cfg.hasRegion) {
        ar.region.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} ARREGION"))
      }
      if (cfg.hasAUser) {
        ar.user.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} ARUSER"))
      }
      ar.valid.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:interface:aximm:1.0 ${interface} ARVALID"))
      ar.ready.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:interface:aximm:1.0 ${interface} ARREADY"))

      r.data.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:interface:aximm:1.0 ${interface} RDATA"))
      r.resp.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:interface:aximm:1.0 ${interface} RRESP"))
      if (cfg.hasId) {
        r.id.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} RID"))
      }
      if (cfg.hasRUser) {
        r.user.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} RUSER"))
      }
      if (cfg.hasBurst) {
        r.last.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:aximm:1.0 ${interface} RLAST"))
      }
      r.valid.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:interface:aximm:1.0 ${interface} RVALID"))
      r.ready.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:interface:aximm:1.0 ${interface} RREADY"))
    }

    interface
  }

  override def isInterface = true
  override def associateClock = true
}

