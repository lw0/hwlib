
# hwlib

Library of SpinalHDL hardware description and simulation utilities


### AXI Interface Definitions

The AXI interface types are derived from an advanced version of the `Bundle` class called `AutoBundle`. This augments the basic functionality of gathering component signals into a single compound signal by a set of assignment policies that establish compatibility between `AutoBundle` instances of slightly differing structures.

As the AMBA standard makes many component signals optional and allows varying bit widths, compatibility between different AXI  ports is very useful.

#### AutoBundle

An `AutoBundle` is bundle of component signals that share the _same direction_ and are annotated (by calling `element("<name>", <clauses>*)`) with assignment and naming policies in the constructor.

```scala
class MyBundle(dataBits : Long, hasFlag : Boolean) extends AutoBundle {
    val data = UInt(dataBits bits)
    val flag = ifGen(hasFlag) { Bool() }
    val addr = UInt(16 bits)
    
    element("data", MayUpsize, WarnDownsize)
    element("flag", MayDefault(True), MayDrop)
    element("addr")
}
```
The example `MyBundle` has an optional component signal `flag`, declared with the `ifGen(cond : Boolean)` construct, which replaces the inner signal with `null` if `cond` is not met. The corresponding policies allow connections between instances with and without the `flag` signal.
The `data` component may have varying bit widths, and the associated policy allows connection when the bit widths do not match. In case the assignment requires `data` to be truncated, a warning is given as information may be lost.
The `addr` component is always present in a `MultiBundle`, so no special handling is required.


Based on the specified policies, `AutoBundle` implements the following methods:


* **`.nameHere(parent, prefix="")`** <br/> Change the name of all component signals to be `"<parentName>_<prefix><signalName>"` flattening nested `AutoBundles` unless the `UsePrefix` policy was used in their definition.
* **`.nameThis(prefix="")`** <br/> `bundle.nameThis` is equivalent to `bundle.nameHere(bundle)`
* **`.connect(from : AutoBundle)`** <br/> Assign this bundle's component signals from the `from` bundle, reacting to mismatches with adaptions, warnings or errors according to the respective assignment policies |
* **`to << from`** <br/> **`from >> to`** <br/> Shortcut for `to.connect(from)`
* **`.assign("name" -> signal*)`** <br/> Assign this bundle's component signals from a name-to-value mapping. Component signals are identified by the name `nameHere("")` would have chosen. In contrast to `.connect()` this method always assigns default values if necessary and never leaves component signals unassigned.
* **`asRegInit("name" -> signal*)`** <br/> Shortcut to make the bundle a register initialized according to the given signal mapping.
* **`cloneAssign("name" -> signal*)`** <br/> Shortcut to create a new combinatorial bundle with the same form of the original one and assign the given signal mapping.
* **`cloneRegInit("name" -> signal*)`** <br/> Shortcut to create a new register bundle with the same form of the original one, initialized with the given signal mapping.

Possible policy clauses that can be passed to `element()` are the following:
| Clause | Semantics |
| ------ | --------- |
| `NoPrefix` | _default_ Omit the name of a nested bundle in the names of its component signals |
| `UsePrefix` | Use the name of a nested bundle in the names of its component signals |
| `UsePrefix(str)` | Use `str` in the names of all nested component signals |
| --- | --- |
| `NoXXX` | _default_ Do not perform a particular transformation automatically but throw an error instead. |
| `MayXXX` | A particular transformation may be performed silently |
| `WarnXXX` | A particular transformation may be performed but a warning is issued as well. |
| `ForbidXXX` | Explicitly forbid a particular transformation. Only useful for nested bundles. |
| `SilentXXX` | Explicitly disable warnings for a particular transformation. Only useful for nested bundles. |
| --- | --- |
| `XXXDefault` | Use a zero default value when assigning a component signal that is missing a counterpart. |
| `XXXDefault(signal)` | Use the value `signal` when assigning a components signal that is missing a counterpart. 
| `XXXIgnore` | Leave a component signal that is missing a counterpart unassigned. Only effective for `.connect()`, `.assign()` will still use a zero default value. |
| `XXXDrop` | Allow ignoring this component signal when there is no counterpart for it to be assigned to |
| `XXXUpsize` | Extend a value when assigning to a wider component signal |
| `XXXDownsize` | Truncate a value when assigning to a narrower component signal |
| `XXXResize` | Combines `XXXUpsize` and `XXXDownsize` |

The `AutoBundleBidir` class extends `AutoBundle` to handle component signals of both directions and implements the SpinalHDL predefined `IMasterSlave` trait. (→ [SpinalHDL Port Directions](https://spinalhdl.github.io/SpinalDoc-RTD/master/SpinalHDL/Structuring/components_hierarchy.html#input-output-definition))
The `element("name", reverseDir, clause*)` method is extended to capture whether the component signal has the same (`false`) or reverse (`true`) direction w.r.t. the bundle's natural direction. Port declarations with `master()` or `slave()` automatically respect the specified directions, as does the `.connect()` method.
The `.assign(reverseDir, "name" -> signal*)` method requires the additional information on which partition of the component signals should be assigned to. The remaining shortcut methods are generally only useful operating on bundles with a single unique signal direction.

#### AxiConfig and TAxi

The `TAxi(cfg : TAxiConfig)` parameterized type can be configured to match a variety of signal options as described in the [AMBA4 AXI Specification](https://developer.arm.com/docs/ihi0022/latest). In particular it can represent an AXI as well as an AXI Lite interface by omitting signals that are disallowed in the AXI Lite subset (e.g. `hasBurst=false`).


| `AxiConfig()` Argument | Default | Semantics |
| ---------------------- | ------- | --------- |
| `addrBits` | required | Bit width of the byte address signals `{aw,ar}addr` |
| `dataBytes` | required | _Byte width_ of the `{w,r}data` signals. Also the bit width of the `wstrb` signal if present as it has a mask bit per data byte |
| `hasStrb` | `true` | Include the `wstrb` signal, controling which bytes of a transfer are written. |
| `hasBurst` | `true` | Include burst control signals (`{aw,ar}len`, `{aw,ar}size`, `{aw,ar}burst` and `{w,r}last`), to handle multi-transfer read and write transactions. Must be `false` for AXI Lite interfaces. |
| `hasAux` | `false` | Include auxilliary transaction attributes on the address channels (`{aw,ar}lock`, `{aw,ar}cache`, `{aw,ar}prot`, `{aw,ar}qos`, `{aw,ar}region`). Can safely be ignored. |
| `hasRd` | `true` | Include the read address and read data channel signals (`ar*` and `r*`). If `false`, no read transactions can be performed on this interface. |
| `hasWr` | `true` | Include the write address, write data and write response channel signals (`aw*`, `w*`, `b*`). If `false`, no write transactions can be performed on this interface. |
| `idBits` | `0` | Bit width of the transaction ID signals (`{aw,b,ar,r}id`) used to specify more multiple transaction ordering groups. If `0`, the signals are omitted alltogether. |
| `{a,w,b,r}UserBits` | `0` | Bit widths of the User signals per channel type, which can carry additional information with application specific semantics. These can safely be ignored. If `0`, the respective signals are omitted alltogether. |
| `defId` <br/> `def{A,W,B,R}User` <br/> `defLock` <br/> `defCache` <br/> `defProt` <br/> `defQos` <br/> `defRegion` | `null` | Default signal values for optional signals to be used when the actual component signals are not available, e.g. when connecting a `TAxi` that lacks these signals or using `.assign("name" -> signal)` without them. See the `AutoBundle` [description](#autobundle) |

Most of the above options have sensible default values, so it is usually sufficient to use `AxiConfig(addrBits=<a>, dataBytes=<d>)` for a plain AXI or `AxiConfig(addrBits=<a>, dataBytes=<d>, hasBurst=false)` for an AXI Lite interface.

#### Simulation Environment

Simulating the complicated interactions between the various AXI channels can be a tedious task if generality w.r.t. the protocol specification is desired.

For that reason, the project already includes predefined simulation objects to encapsulate this behavior.

* `ClockEnv` - Wraps a simulation ClockDomain and counts clock cycles as a timing reference for other simulation models.
* `AxiWrMaster` - Connects to the write half (AW, W and B channels) of an AXI slave port on the `dut` and offers an API to initiate write transactions returning `Futures` (using the `write*()` methods) or blocking until the transaction completes (using the `bwrite*()` methods)
* `AxiRdMaster` - Connects to the read half (AR, R channels) of an AXI slave port on the `dut` and offers an API to initiate read transactions returning `Futures` (using the `read*()` methods) or blocking until the transaction completes (using the `bread*()` methods)
* `AxiWrSlave` - Connects to the write half of an AXI master port and reacts to write transactions on it. With `onWrite(base : Long, count : Long)` a handler function for writes to the address range `[base, base + count - 1]` can be registered.
* `AxiRdSlave` - Connects to the read half of an AXI master port and reacts to read transactions on it. With `onRead(base : Long, count : Long)` a handler function for reads to the address range `[base, base + count - 1]` can be registered.
* `AxiModelConfig` - Controls the timing and ordering behavior of an `AxiWrMaster`, `AxiRdMaster`, `AxiWrSlave`, `AxiRdSlave` model.

The `design/Scratch.scala` source file (see the relevant extract below) gives a showcase of how these simulation models can be employed in a very simple passthrough application:
The `Scratch.io.sMem` master port is simply buffered and connected to the `Scratch.io.mMem` port. The `Scratch_TB` testbench handles transactions on `mAxi` using the `Axi{Wr,Rd}Slave` models with a memory buffer, and initiates test transactions on `sMem` through the `Axi{Wr,Rd}Master` models.

```scala
    model.doSim { dut =>
      // Setup Clock Domain
      implicit val env = new ClockEnv(dut.clockDomain, 5000)
      env.onRawCycle(2) { () =>
        env.setRst(false)
      }
      
      // Create AXI simulation models
      val masterCfg = AxiModelConfig()
      val slaveCfg = AxiModelConfig()
      val iWrMaster = new AxiWrMaster(dut.io.sMem, masterCfg)
      val iRdMaster = new AxiRdMaster(dut.io.sMem, masterCfg)
      val iWrSlave = new AxiWrSlave(dut.io.mMem, slaveCfg)
      val iRdSlave = new AxiRdSlave(dut.io.mMem, slaveCfg)

      // Attach a memory buffer to handle requests on dut.io.mMem
      val iMemBuf = new MemBuffer(12) // 2^12 = 4KB pages
      iWrSlave.onWrite(0x000, 0x1000) {
        (addr, bytes, data, mask) =>
          iMemBuf.writeMask(addr, bytes, data, mask)
          Resp.Okay
      }
      iRdSlave.onRead(0x000, 0x1000) {
        (addr, bytes) =>
          val data = iMemBuf.read(addr, bytes)
          (Resp.Okay, data)
      }

      // Generate AXI traffic
      env.clk.waitSampling()
      println("=== Begin ===")
      val pendingWrite = iWrMaster.write(0x100, 0x6c)
      val pendingRead = iRdMaster.read(0x104)
      syncWrite(s"write(0x100, 0x6c) -> ", pendingWrite)
      syncRead(s"read(0x104) -> ", pendingRead)
      println("=== End ===")
      env.clk.waitSampling(32)
    }
  }
```

An `AxiModelConfig` instance is used to package all information required by an AXI simulation model to control its timing and ordering behavior. To add some variety and realism to the simulations, some timing parameters are not fixed numbers but number generators. The predefined `Rate(meanRate : Double) : () => Long` and `Delay(meanDelay : Double) : () => Long` generate random sequences of delay cycle values corresponding either to an average activity rate or delay. These delay sequences follow a [Poisson Distribution](https://en.wikipedia.org/wiki/Poisson_distribution). The `Instant()` generator always returns value `0L` disabling any cycle delay.

| `AxiModelConfig` Field | Default | Semantics |
| ------------------------ | ------- | --------- |
| `issueDelay` | `Instant()` | Cycles since initation before processing a read / write request may begin |
| `issueRate` | `Rate(1.0)` | Delay between processing of subsequent read / write requests. |
| `issueReorder` | `false` | Allow reordering read / write requests w.r.t. issue order. |
| `issuePrio` | `false` | Prioritize read / write requests of lower `id` values. Otherwise requests are handled round robin over all available `id` values. |
| `resolveDepth` | `0` | Capacity of the queue storing pending requests. `0` means no capacity limit. |
| `resolveDelay` | `Instant()` | Cycles since start of processing a read / write request before it can be completed. |
| `resolveRate` | `Rate(1.0)` | Cycles between completion of subsequent read / write requests. |
| `addrCap` | `0` | Capacity of the address request (AW or AW channel) queues. `0` means no capacity limit. |
| `dataCap` | `0` | Capacity of the data (W or R channel) queues. `0` means no capacity limit. |
| `respCap` | `0` | Capacity of the response (B channel) queue. |
| `addrRate` | `Rate(1.0)` | Delay between subsequent address requests. |
| `dataRate` | `Rate(1.0)` | Delay between subsequent data transfers. |
| `respRate` | `Rate(1.0)` | Delay between subsequent responses. |

Though you need not go deeper into the inner workings of the simulation framework, here is a brief overview of the classes used in the background:
* `APack`, `WPack`, `BPack`, `RPack` - Abstract data representations capturing the relevant information pertaining to an address, write data, write response or read data channel transfer. Each class comes with `read()` and `drive()` functions to convert actual AXI channel signals to the internal representation. `APack` is especially interesting, as it additionally implements the burst behavior as specified in the [AMBA4 AXI Specification](https://developer.arm.com/docs/ihi0022/latest)
* `ChannelSender[T,U](ch : Channel[T], cfg : DelayQueueConfig) { conv }` - Handles the raw interaction to send data on an AMBA channel, by driving the payload `T` signals with custom conversion code `conv` from an abstract data representation `U`. Timing and ordering behavior is controlled by a `DelayQueueConfig`.
* `ChannelReceiver[T,U](ch : Channel[T], cfg : DelayQueueConfig) { conv }` - Handles the raw interaction to receive data from an AMBA channel, by sampling the payload `T` signals with custom conversion code `conv` to an abstract data representation `U`. Timing and ordering behavior is controlled by a `DelayQueueConfig`.
* `DelayQueue[T](cfg : DelayQueueConfig)` - Fundamental simulation model to capture the delayed flow or processing of data items. Incoming and outgoing data rates, per-item latencies, capacity limits and ordering behavior can be configured.


#### Source File Index

* `design/util/AutoBundle.scala` - <br/> **`AutoBundle`** - an extended `Bundle` with automatic assignment, connection, and rename methods; component signals all have the same direction<br/> **`AutoBundleBidir`** - same as `AutoBundle` but allows component signals in reverse direction by implementing `IMasterSlave` <br/> **`AutoPolicyClause`** - Specify desired behavior when resolving mismatches between assigned / connected component signals |
* `design/amba/Amba.scala` - <br/> **`Amba`** - a singleton object defining common types and constants used in various AMBA protocols
* `design/amba/Channel.scala` - <br/> **`Channel[T]`** - a generic AMBA Channel with a `payload : T` and `valid` / `ready` handshake signals
* `design/amba/Axi.scala` - <br/> **`AxiConfig`** - configuration object encapsulating all parameters and options to define a concrete `TAxi` type. <br/> **`TAxi`** - the main AXI interface type composed of the `Channel` derivatives **`TAxiA`**, **`TAxiW`**, **`TAxiB`** and **`TAxiR`**, each carrying a **`TAxi{A,W,B,R}Payload`**.
* `design/sim/ClockEnv.scala` - <br/> **`ClockEnv`** - A wrapper for a `ClockDomain` in simulation, driving the clock and reset signals in the background, counting clock cycles and emitting events at definable cycle counts.
* `design/sim/Debug.scala` - <br/> **`Debug`** - allows registration of a hierarchy of simulation model objects under an identifier string. Sending debug messages via `printFor(simObject, message)` annotates the message with the current cycle count as well as the named path of the originating object. A common instance is generally available under `ClockEnv.dbg`.
* `design/sim/Future.scala` - <br/> **`Future[T]`** - a synchronization primitive between simulation threads: Encapsulates a return value that may not be available yet. Besides querying the availability, it allows suspending the current thread until the value is available using `blockValue() : T`. <br/> **`FutureSet`** - Combines multiple futures and blocks until any one or all are available.
* `design/sim/PoissonDist.scala` - <br/> **`PoissonDist(lambda : Double, offset : Long)`** - Generate a sequence of `Long` values corresponding to a [Poisson Distribution](https://en.wikipedia.org/wiki/Poisson_distribution). <br/> **`Rate(meanRate : Double)`** - Generate a sequence of delay values that result on an average activity rate of `meanRate` <br/> **`Delay(meanDelay : Double)`** - Generate a sequence of delay values that result on an average `meanDelay` <br/> **`Instant`** - Generate a sequence of `0L` values, corresponding to no delays.
* `design/sim/DelayQueue.scala` - <br/> **`DelayQueue[T](cfg : DelayQueueConfig)`** - A simulation model representing the delayed flow of data items. Incoming and outgoing bandwidths, per-item latencies, capacity limits and ordering behavior can be configured. 
* `design/sim/MemBuffer.scala` - <br/> **`MemBuffer(pageLogSize : Int)`** - a sparse map to represent a piece of memory. Allocation happens in units of pages with configurable size.
* `design/sim/RangeMap.scala` - <br/> **`RangeMap[T]`** - Map values, usually addresses, to the most specific of registered ranges, as specified by a `base : Long, count : Long` pair. Not a very efficient implementation...
* `design/sim/model/ChannelSender.scala` - <br/>  **`ChannelSender[T,U](ch: Channel[T], cfg : DelayQueueConfig)`** - Simulation model sending data to a `Channel` with configurable timing and ordering behavior. Requires a conversion function from the abstract data representation `U` to apply to the SpinalHDL signal type `T`
* `design/sim/model/ChannelReceiver.scala` - <br/> **`ChannelReceiver[T,U](ch : Channel[T], cfg : DelayQueueConfig)`** - Simulation model receiving data from a `Channel` with configurable behavior. Requires a conversion function to read the SpinalHDL signal type `T` into an abstract data representation `U`
* `design/sim/model/AxiModelConfig.scala` - <br/> **`AxiModelConfig`** - Control the timing and ordering behavior of an AXI Model (`AxiWrMaster`, `AxiRdMaster`, `AxiWrSlave`, `AxiRdSlave`).
* `design/sim/model/AxiWrMaster.scala` - <br/> **`AxiWrMaster`** - Simulation model controlling an AXI (Lite) slave interface, offering an API to initiate write requests.
* `design/sim/model/AxiRdMaster.scala` - <br/> **`AxiRdMaster`** - Simulation model controlling an AXI (Lite) slave interface, offering an API to initiate read requests.
* `design/sim/model/AxiWrSlave.scala` - <br/> **`AxiWrSlave`** - Simulation model controlled by an AXI (Lite) master interface, allowing registration of callbacks to handle write requests.
* `design/sim/model/AxiRdSlave.scala` - <br/> **`AxiRdSlave`** - Simulation model controlled by an AXI (Lite) master interface, allowing registration of callbacks to handle read requests.
* `design/sim/model/amba/APack.scala` - <br/> **`APack`** - Representation of address channel transfer information.
* `design/sim/model/amba/WPack.scala` - <br/> **`WPack`** - Representation of write data channel transfer information.
* `design/sim/model/amba/BPack.scala` - <br/> **`BPack`** - Representation of write response channel transfer information.
* `design/sim/model/amba/RPack.scala` - <br/> **`RPack`** - Representation of read data channel transfer information.
* `design/sim/model/amba/enums.scala` - <br/> **`Resp`** - Enumeration of valid responses. <br/> **`Burst`** - Enumeration of valid burst types. <br/> **`Size`** - Enumeration of valid burst transfer sizes.

