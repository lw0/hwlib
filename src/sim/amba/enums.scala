package hwlib.sim.amba

import spinal.core._



object Resp extends Enumeration {
  type Resp = Resp.Value
  def min(l : Value, r : Value) = ValueOrdering.min(l, r)
  def max(l : Value, r : Value) = ValueOrdering.max(l, r)
  val Okay, ExOkay, SlvErr, DecErr = Value
}


object Burst extends Enumeration {
  type Burst = Burst.Value
  def min(l : Value, r : Value) = ValueOrdering.min(l, r)
  def max(l : Value, r : Value) = ValueOrdering.max(l, r)
  val Fixed, Incr, Wrap = Value
}


object Size extends Enumeration {
  type Size = Size.Value
  def min(l : Value, r : Value) = ValueOrdering.min(l, r)
  def max(l : Value, r : Value) = ValueOrdering.max(l, r)
  val S1B, S2B, S4B, S8B, S16B, S32B, S64B, S128B = Value
  def from(logSize : Int) : Size = {
    if (logSize < 0) {
      SpinalWarning(s"Invalid Size.from(${logSize}), defaulting to Size.S1B")
      S1B
    } else if (logSize > 7) {
      SpinalWarning(s"Invalid Size.from(${logSize}), defaulting to Size.S128B")
      S128B
    } else {
      apply(logSize)
    }
  }
}
