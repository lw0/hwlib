package hwlib.sim.amba

import hwlib.sim.{DelayQueueConfig, RateGen, DelayGen, InstantGen}


case class AxiModelConfig(
  issueDelay : () => Long = InstantGen(),
  issueRate : () => Long = RateGen(1.0),
  issueReorder : Boolean = false,
  issuePrio : Boolean = false,
  resolveDepth : Int = 0,
  resolveDelay : () => Long = InstantGen(),
  resolveRate : () => Long = RateGen(1.0),
  addrCap : Int = 0,
  addrRate  : () => Long = RateGen(1.0),
  dataCap : Int = 0,
  dataRate  : () => Long = RateGen(1.0),
  respCap : Int = 0,
  respRate  : () => Long = RateGen(1.0)) {
  val aSendCfg = DelayQueueConfig(
    dGet = addrRate,
    capacity = addrCap)
  val aRecvCfg = DelayQueueConfig(
    dPut = addrRate,
    capacity = addrCap)
  val dSendCfg = DelayQueueConfig(
    dGet = dataRate,
    capacity = dataCap)
  val dRecvCfg = DelayQueueConfig(
    dPut = dataRate,
    capacity = dataCap)
  val bSendCfg = DelayQueueConfig(
    dGet = respRate,
    capacity = respCap)
  val bRecvCfg = DelayQueueConfig(
    dPut = respRate,
    capacity = respCap)
  val qProcCfg = DelayQueueConfig(
    dPut = issueRate,
    dLat = resolveDelay,
    dGet = resolveRate,
    prio = issuePrio,
    capacity = resolveDepth)
  val qIssCfg = DelayQueueConfig(
    dLat = issueDelay,
    dGet = issueRate,
    reorder = issueReorder,
    prio = issuePrio)
  val qResCfg = DelayQueueConfig(
    dLat = resolveDelay,
    dGet = resolveRate,
    capacity = resolveDepth)
}


