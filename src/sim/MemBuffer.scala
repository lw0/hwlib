package hwlib.sim

import scala.collection.mutable.{Map, StringBuilder}
import scala.math.BigInt
import scala.Array


class MemBuffer(val pageLogSize : Int) {
  require(pageLogSize < 32)
  val pageBytes = 1 << pageLogSize
  val pageMask = pageBytes.toLong - 1L

  val pages = Map[Long, Array[Byte]]()

  case class PageRef(num : Long, page : Array[Byte], off : Int, len : Int)

  def pageRef(addr : Long, bytes : Int = pageBytes) : PageRef = {
    val num = addr & ~pageMask
    val off = (addr & pageMask).toInt
    val len = (pageBytes - off) min bytes
    if (!pages.contains(num)) {
      pages(num) = Array.fill(pageBytes)(0.toByte)
    }
    PageRef(num, pages(num), off, len)
  }

  def writeMask(addr : Long, bytes : Int, data : BigInt, mask : BigInt, be : Boolean = false) : Unit = {
    val ref = pageRef(addr, bytes)
    val darray = (if (be) data.toByteArray else data.toByteArray.reverse).padTo(bytes, 0.toByte)
    for (i <- 0 until ref.len) {
      if (mask.testBit(i)) {
        ref.page(ref.off + i) = darray(i)
      }
    }
  }

  def write(addr : Long, bytes : Int, data : BigInt, be : Boolean = false) : Unit = {
    writeMask(addr, bytes, data, (BigInt(1) << bytes) - 1, be)
  }

  def read(addr : Long, bytes : Int, be : Boolean = false) : BigInt = {
    val ref = pageRef(addr, bytes)
    val arr = ref.page.slice(ref.off, ref.off + ref.len)
    BigInt(1, if (be) arr else arr.reverse)
  }

  def writeByte(addr : Long, data : Byte) = {
    write(addr, 1, data)
  }

  def readByte(addr : Long) : Byte = {
    read(addr, 1).toByte
  }

  def writeShort(addr : Long, data : Short, mask : Int = 0x3, be : Boolean = false) = {
    writeMask(addr, 2, data, mask, be)
  }

  def readShort(addr : Long, be : Boolean = false) : Short = {
    read(addr, 2, be).toShort
  }

  def writeInt(addr : Long, data : Int, mask : Int = 0xf, be : Boolean = false) = {
    writeMask(addr, 4, data, mask, be)
  }

  def readInt(addr : Long, be : Boolean = false) : Int = {
    read(addr, 4, be).toInt
  }

  def writeLong(addr : Long, data : Long, mask : Int = 0xff, be : Boolean = false) = {
    writeMask(addr, 8, data, mask, be)
  }

  def readLong(addr : Long, be : Boolean = false) : Long = {
    read(addr, 8, be).toLong
  }

  def dump(addr : Long, bytes : Int = pageBytes, width : Int = 16) : String = {
    val ref = pageRef(addr, bytes)
    val rbeg = ref.off
    val rend = ref.off + ref.len - 1
    val bpad = rbeg % width
    val epad = rend % width
    val beg = rbeg / width
    val end = rend / width
    val sbuf = new StringBuilder(f"Page ${ref.num}%016x + ${pageBytes-1}%08x\n")
    for (i <- beg to end) {
      val loff = i * width
      val laddr = ref.num + loff
      val jBeg = if (i == beg) bpad else 0
      val jEnd = if (i == end) epad else width - 1
      val row = (0 until jBeg).map((j) => "  ") ++
                (jBeg to jEnd).map((j) => f"${ref.page(loff + j)}%02x") ++
                ((jEnd+1) until width).map((j) => "  ")
      sbuf ++= f"${laddr}%016x: ${row.mkString(" ")}\n"
    }
    sbuf.result()
  }

  def dumpAll(detail : Boolean = false, width : Int = 16) : String = {
    val sbuf = new StringBuilder()
    pages.keys.toList.sorted.foreach {
      (pnum) =>
        if (detail) {
          sbuf ++= dump(pnum, width=width)
        } else {
          sbuf ++= f"Page ${pnum}%016x + ${pageBytes-1}%08x\n"
        }
    }
    sbuf.result()
  }
}

