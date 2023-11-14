package hwlib.base

import spinal.core._


object Mask {
  // RightZeros(U(3), 8) -> 0b11111000
  def RightZeros(zeros : UInt, width : Int)  : Bits = Bits(width bits).setAll() |<< zeros
  def RightZerosU(zeros : UInt, width : Int) : UInt = RightZeros(zeros, width).asUInt
  def RightZerosS(zeros : UInt, width : Int) : SInt = RightZeros(zeros, width).asSInt

  // RightOnes(U(3), 8) -> 0b00000111
  def RightOnes(ones : UInt, width : Int)    : Bits = ~RightZeros(ones, width)
  def RightOnesU(ones : UInt, width : Int)   : UInt = RightOnes(ones, width).asUInt
  def RightOnesS(ones : UInt, width : Int)   : SInt = RightOnes(ones, width).asSInt


  // LeftZeros(U(3), 8) -> 0b00011111
  def LeftZeros(zeros : UInt, width : Int)  : Bits = Bits(width bits).setAll() |>> zeros
  def LeftZerosU(zeros : UInt, width : Int) : UInt = LeftZeros(zeros, width).asUInt
  def LeftZerosS(zeros : UInt, width : Int) : SInt = LeftZeros(zeros, width).asSInt

  // LeftOnes(U(3), 8) -> 0b11100000
  def LeftOnes(ones : UInt, width : Int)    : Bits = ~LeftZeros(ones, width)
  def LeftOnesU(ones : UInt, width : Int)   : UInt = LeftOnes(ones, width).asUInt
  def LeftOnesS(ones : UInt, width : Int)   : SInt = LeftOnes(ones, width).asSInt


  // BitOne(U(3), 8) -> 0b00001000
  def BitOne(bit : UInt, width : Int)   : Bits = if(width > 0) { B(1, width bits) |<< bit } else { B(0, width bits) }
  def BitOneU(bit : UInt, width : Int)  : UInt = BitOne(bit, width).asUInt
  def BitOneS(bit : UInt, width : Int)  : SInt = BitOne(bit, width).asSInt

  // BitZero(U(3), 8) -> 0b11110111
  def BitZero(bit : UInt, width : Int)  : Bits = ~BitOne(bit, width)
  def BitZeroU(bit : UInt, width : Int) : UInt = BitZero(bit, width).asUInt
  def BitZeroS(bit : UInt, width : Int) : SInt = BitZero(bit, width).asSInt


  // SpanOnes(U(3), U(6), 8) -> 0b00111000 = 0b00111111 & 0b11111000
  def SpanOnes(to : UInt, from : UInt, width : Int)   : Bits = RightOnes(to, width) & RightZeros(from, width)
  def SpanOnesU(to : UInt, from : UInt, width : Int)  : UInt = SpanOnes(to, from, width).asUInt
  def SpanOnesS(to : UInt, from : UInt, width : Int)  : SInt = SpanOnes(to, from, width).asSInt

  // SpanZeros(U(3), U(6), 8) -> 0b11000111 = 0b11000000 | 0b00000111
  def SpanZeros(to : UInt, from : UInt, width : Int)  : Bits = RightZeros(to, width) | RightOnes(from, width)
  def SpanZerosU(to : UInt, from : UInt, width : Int) : UInt = SpanZeros(to, from, width).asUInt
  def SpanZerosS(to : UInt, from : UInt, width : Int) : SInt = SpanZeros(to, from, width).asSInt


  // RangeOnes(U(2), U(4), 8) -> 0b00111100
  def RangeOnes(offset : UInt, length : UInt, width : Int)   : Bits = RightOnes(length, width) |<< offset
  def RangeOnesU(offset : UInt, length : UInt, width : Int)  : UInt = RangeOnes(offset, length, width).asUInt
  def RangeOnesS(offset : UInt, length : UInt, width : Int)  : SInt = RangeOnes(offset, length, width).asSInt

  // RangeZeros(U(2), U(4), 8) -> 0b11000011
  def RangeZeros(offset : UInt, length : UInt, width : Int)  : Bits = ~RangeOnes(offset, length, width)
  def RangeZerosU(offset : UInt, length : UInt, width : Int) : UInt = RangeZeros(offset, length, width).asUInt
  def RangeZerosS(offset : UInt, length : UInt, width : Int) : SInt = RangeZeros(offset, length, width).asSInt
}

object HexString {
  def apply(num : BigInt, unit : Int = 2, group : Int = 0, unitSep : String = " ", groupSep : String = "  ") = {
    val str = num.toString(16)
    val units = str.reverse.grouped(unit).map(_.reverse)
    if (group > 0) {
      units.grouped(group).map(_.mkString(unitSep)).mkString(groupSep)
    } else {
      units.mkString(unitSep)
    }
  }

}
