package hwlib.sim

import scala.collection.mutable.{PriorityQueue, Queue, Map}
import scala.util.Random
import scala.math._

import spinal.core._
import spinal.sim._
import spinal.core.sim._


case class GetAction(val take : Boolean, val touch : Boolean)
object Take extends GetAction(true, true)
object Touch extends GetAction(false, true)
object Leave extends GetAction(false, false)

case class DelayQueueConfig(
  val dPut : () => Long = InstantGen(),
  val dLat : () => Long = InstantGen(),
  val dGet : () => Long = InstantGen(),
  val capacity : Int = 0,
  val reorder : Boolean = false,
  val prio : Boolean = false)


class DelayQueueModel[T](val cfg : DelayQueueConfig)(implicit val env : ClockEnv) {

  private case class Item(item : T, idx : Long, at : Long) extends Ordered[Item] {
    def compare(that : Item) = if (cfg.reorder) {that.at compare this.at} else {that.idx compare this.idx}
  }

  private val qmap = Map(0L -> PriorityQueue[Item]())
  private var index = 0L
  private var nextPutAt = 0L
  private var nextGetAt = 0L
  private var lastGetId = -1L

  private def queue(id : Long) : PriorityQueue[Item] = {
    if (!qmap.contains(id)) {
      qmap(id) = PriorityQueue[Item]()
    }
    qmap(id)
  }

  private def chooseGet(id : Option[Long], in : Long = 0) : Option[(Long, PriorityQueue[Item])] = {
    if (nextGetAt > (env.cycles + in)) {
      None
    } else {
      qmap.toList
          .filter((e) => id.isEmpty || e._1 == id.get)
          .sortBy((e) => if (cfg.prio) e._1 else floorMod(e._1 - lastGetId - 1, qmap.keys.max + 1))
          .find((e) => e._2.nonEmpty && e._2.head.at <= (env.cycles + in))
    }
  }

  def size = qmap.map(_._2.size).sum

  def clear() : Unit = {
    qmap.filterInPlace((id, q) => id == 0)
    qmap(0).clear()
    index = 0L
    nextPutAt = 0
    nextGetAt = 0
    lastGetId = -1
  }

  def canPut(in : Long = 0) : Boolean = {
    (cfg.capacity == 0 || size < cfg.capacity) && nextPutAt <= (env.cycles + in)
  }

  def put(item : T, id : Long) : Boolean = {
    if (canPut()) {
      doPut(item, id)
      true
    } else {
      false
    }
  }

  def putWith(call : () => Option[(T, Long)]) : Boolean = {
    if (canPut()) {
      val res = call()
      if (res.isDefined) {
        doPut(res.get._1, res.get._2)
      }
      res.isDefined
    } else {
      false
    }
  }

  private def doPut(item : T, id : Long) : Unit = {
    nextPutAt = env.cycles + cfg.dPut()
    queue(id).enqueue(Item(item, index, env.cycles + cfg.dLat()))
    index += 1
  }

  def canGet(id : Option[Long], in : Long = 0) : Boolean = {
    chooseGet(id, in).isDefined
  }

  def get(id : Option[Long]) : Option[(T, Long)] = {
    chooseGet(id) match {
      case Some((i, q)) => {
        lastGetId = i
        Some((doGet(i, q), i))
      }
      case _ => None
    }
  }

  def getWith(id : Option[Long])(call : (T, Long) => GetAction) : GetAction = {
    chooseGet(id) match {
      case Some((i, q)) => {
        val res = call(q.head.item, i)
        if (res.take) {
          doGet(i, q)
        }
        if (res.touch) {
          lastGetId = i
        }
        res
      }
      case _ => Leave
    }
  }

  private def doGet(id : Long, queue : PriorityQueue[Item]) : T = {
    nextGetAt = env.cycles + cfg.dGet()
    queue.dequeue().item
  }

}

class DelayQueue[T](val cfg : DelayQueueConfig = DelayQueueConfig())(implicit val env : ClockEnv) {

  private abstract class PutHandler {
    def apply() : Option[(T, Long)]
    def clear() : Unit = {}
  }

  private class CustomPutHandler(val call : () => Option[(T, Long)]) extends PutHandler {
    def apply() : Option[(T, Long)] = call()
  }

  private class FuturePutHandler extends PutHandler {

    case class PutOp(item : T, id : Long, fut : Future[Unit])

    val queue = Queue[PutOp]()

    def apply() : Option[(T, Long)] = {
      if (queue.nonEmpty) {
        val op = queue.dequeue()
        op.fut.resolve()
        Some((op.item, op.id))
      } else {
        None
      }
    }

    override def clear() : Unit = {
      queue.filterInPlace{
        (op) =>
          op.fut.resolve()
          false
      }
    }

    def doPut(item : T, id : Long) : Future[Unit] = {
      val fut = Future[Unit]()
      queue.enqueue(PutOp(item, id, fut))
      fut
    }

  }

  private abstract class GetHandler {
    def apply(item : T, id : Long) : GetAction
    def clear() : Unit = {}
  }

  private class CustomGetHandler(val call : (T, Long) => GetAction) extends GetHandler {
    def apply(item : T, id : Long) : GetAction = call(item, id)
  }

  private class FutureGetHandler extends GetHandler {

    val queue = Queue[Future[Option[T]]]()

    def apply(item : T, id : Long) : GetAction = {
      if (queue.nonEmpty) {
        val fut = queue.dequeue()
        fut.resolve(Some(item))
        Take
      } else {
        Leave
      }
    }

    override def clear() : Unit = {
      queue.filterInPlace{
        (fut) =>
          fut.resolve(None)
          false
      }
    }

    def doGet() : Future[Option[T]] = {
      val fut = Future[Option[T]]()
      queue.enqueue(fut)
      fut
    }
  }

  private val model = new DelayQueueModel[T](cfg)
  private var putHandler : PutHandler = new FuturePutHandler
  private val getIdHandlers = Map[Long, GetHandler]()
  private var getAnyHandler : GetHandler = new FutureGetHandler

  env.dbg.addChild(this, model, "m")
  env.dbg.addChild(this, putHandler, "pH")
  env.dbg.addChild(this, getAnyHandler, "gH")

  env.onClk(() => update())
  env.onRst(() => reset())

  def rawSize = model.size
  def rawCanFit(items : Long) = if (cfg.capacity == 0) {
      true
    } else {
      model.size < (cfg.capacity - items)
    }
  def rawCanSupply(items : Long) = model.size >= items

  // Put APIs:
  //     only one of the following method groups should be used
  //     on a particular instance for consistent behavior
  // - Put Raw
  def rawCanPut(in : Long = 0) : Boolean = model.canPut(in)
  def rawPut(item : T, id : Long = 0) : Boolean = model.put(item, id)
  def putWith(call : () => Option[(T, Long)]) : Boolean = model.putWith(call)

  // - Put Future
  def put(item : T, id : Long = 0) : Future[Unit] = {
    val handler = if (putHandler.isInstanceOf[FuturePutHandler]) {
      Some(putHandler.asInstanceOf[FuturePutHandler])
    } else {
      None
    }
    if (handler.isDefined) {
      val fut = handler.get.doPut(item, id)
      update()
      fut
    } else {
      SpinalWarning(s"DelayQueue: Trying to put() with a custom PutHandler installed, thus ignoring")
      Future[Unit]().resolve()
    }
  }
  def bput(item : T, id : Long = 0) : Unit = put(item, id).block()

  // - Put Handler
  def onPut(call : () => Option[(T, Long)]) : Unit = {
    putHandler = new CustomPutHandler(call)
  }

  def onPutDefault() : Unit = {
    if (!putHandler.isInstanceOf[FuturePutHandler]) {
      putHandler = new FuturePutHandler()
    }
  }

  // Get APIs:
  //     only one of the following method groups should be used
  //     on a particular instance for consistent behavior
  // - Get Raw
  def rawCanGet(id : Option[Long] = None, in : Long = 0) : Boolean = model.canGet(id, in)
  def rawGet(id : Option[Long] = None) : Option[T] = model.get(id).map(_._1)
  def getWith(id : Option[Long])(call : (T, Long) => GetAction) : GetAction = model.getWith(id)(call)

  // - Get Future
  def get(id : Option[Long] = None) : Future[Option[T]] = {
    val handler = if (id.isDefined) {
      if (getIdHandlers.contains(id.get)) {
        val handler = getIdHandlers(id.get)
        if (handler.isInstanceOf[FutureGetHandler]) {
          Some(handler.asInstanceOf[FutureGetHandler])
        } else {
          None
        }
      } else {
        val handler = new FutureGetHandler
        getIdHandlers(id.get) = handler
        Some(handler)
      }
    } else {
      if (getAnyHandler.isInstanceOf[FutureGetHandler]) {
        Some(getAnyHandler.asInstanceOf[FutureGetHandler])
      } else {
        None
      }
    }
    if (handler.isDefined) {
      val fut = handler.get.doGet()
      update()
      fut
    } else {
      SpinalWarning(s"DelayQueue: Trying to get(${id}) with a custom GetHandler installed, thus ignoring")
      Future[Option[T]]().resolve(None)
    }
  }
  def bget(id : Option[Long] = None) : Option[T] = get(id).blockValue()

  // - Get Handler
  def onGet(id : Option[Long] = None)(call : (T, Long) => GetAction) : Unit = {
    if (id.isDefined) {
      getIdHandlers(id.get) = new CustomGetHandler(call)
    } else {
      getAnyHandler = new CustomGetHandler(call)
    }
  }

  def onGetDefault(id : Option[Long] = None) : Unit = {
    if (id.isDefined) {
      if(getIdHandlers.get(id.get).map(!_.isInstanceOf[FutureGetHandler]).getOrElse(false)) {
        getIdHandlers(id.get) = new FutureGetHandler
      }
    } else {
      if (!getAnyHandler.isInstanceOf[FutureGetHandler]) {
        getAnyHandler = new FutureGetHandler
      }
    }
  }


  // --- Internals ---
  private def update() : Unit = {
    var delta = true
    while (delta) {
      // first prio: get specific id
      delta = getIdHandlers.exists {
          (entry) =>
            model.getWith(Some(entry._1))(entry._2.apply).take
      }
      // second prio: get any id
      if (!delta) {
        delta = model.getWith(None)(getAnyHandler.apply).take
      }
      // third prio: put
      if (!delta) {
        delta = model.putWith(putHandler.apply)
      }
    }
  }

  private def reset() : Unit = {
    model.clear()
    putHandler.clear()
    getIdHandlers.foreachEntry((id, handler) => handler.clear())
    getAnyHandler.clear()
  }
}
