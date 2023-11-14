package hwlib.sim

import scala.collection.mutable.ArrayBuffer
import scala.math.BigInt


class RangeMap[T] {

  private case class Range(base : Long, count : Long, item : T) extends Ordered[Range] {
    def compare(other : Range) : Int = {
      return count compare other.count
    }

    def end = base + count - 1

    def contains(point : Long) : Boolean = {
      base <= point && point < (base + count)
    }

    def relate(range : Range) : (Boolean, Boolean, Boolean) = {
      (contains(range.base), contains(range.end), range.contains(base))
    }

    def contains(range : Range) : Boolean = {
      relate(range) match {
        case (true, true, false) => true
        case _ => false
      }
    }

    def contained(range : Range) : Boolean = {
      relate(range) match {
        case (false, false, true) => true
        case _ => false
      }
    }

    def intersects(range : Range) : Boolean = {
      relate(range) match {
        case (true, false, false) => true
        case (false, true, true) => true
        case _ => false
      }
    }

    def overlaps(range : Range) : Boolean = {
      relate(range) match {
        case (false, false, false) => false
        case _ => true
      }
    }
  }

  private val ranges = ArrayBuffer[Range]()

  def add(base : Long, count : Long, item : T) : Boolean = {
    val range = Range(base, count, item)
    val intersect = ranges.find((r) => r.intersects(range))
    if (intersect.isEmpty) {
      ranges.addOne(range)
      true
    } else {
      false
    }
  }

  def get(point : Long) : Option[T] = {
    ranges
      .filter((r) => r.contains(point))
      .sorted
      .headOption
      .map(_.item)
  }

}

