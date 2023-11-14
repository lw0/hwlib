package hwlib.amba

import spinal.core._



object Amba {
  def TResp   = Bits(2 bits)
  def TBurst  = Bits(2 bits)
  def TSize   = Bits(3 bits)
  def TLock   = Bits(2 bits)
  def TCache  = Bits(4 bits)
  def TProt   = Bits(3 bits)
  def TQos    = Bits(4 bits)
  def TRegion = Bits(4 bits)

  def cRespOkay      = B"00"
  def cRespExOkay    = B"01"
  def cRespSlvErr    = B"10"
  def cRespDecErr    = B"11"

  def cBurstFixed    = B"00"
  def cBurstIncr     = B"01"
  def cBurstWrap     = B"10"

  def cLenSingle     = U"8'0"

  def cLockNormal    = B"00"
  def cLockExcl      = B"01"
  def cLockLocked    = B"10"

  def cCacheNone     = B"0000"
  def cCacheFBuffer  = B"0001"
  def cCacheFModify  = B"0010"
  def cCacheFWrAlloc = B"0100"
  def cCacheFWrOther = B"1000"
  def cCacheFRdOther = B"0100"
  def cCacheFRdAlloc = B"1000"

  def cProtNone      = B"000"
  def cProtFPriv     = B"001"
  def cProtFNSec     = B"010"
  def cProtFInst     = B"100"

  def cQosNone       = B"0000"

  def cRegionNone    = B"0000"
}

