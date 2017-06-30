// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.groundtest

import Chisel._
import freechips.rocketchip.chip._
import freechips.rocketchip.coreplex._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile.TileKey
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

/** Actual testing target Configs */

class GroundTestConfig extends Config(new WithGroundTestTiles ++ new BaseConfig)

class TraceGenConfig extends Config(
  new WithTraceGen(2) ++ new GroundTestConfig)

class TraceGenBufferlessConfig extends Config(
  new WithBufferlessBroadcastHub ++ new TraceGenConfig)

class TraceGenL2Config extends Config(
  new WithNL2Ways(1) ++ new WithL2Capacity(32 * 64 / 1024) ++
  new WithL2Cache ++ new TraceGenConfig)

/* Composable Configs to set individual parameters */

class WithGroundTestTiles extends Config((site, here, up) => {
  case TileKey => site(GroundTestKey).head
  case NTiles => site(GroundTestKey).size
})

class WithTraceGen(n: Int) extends Config((site, here, up) => {
  case GroundTestKey => Seq.fill(n) {
    GroundTestTileParams(dcache = Some(DCacheParams(nSets = 16, nWays = 1)))
  }
  case BuildGroundTest =>
    (p: Parameters) => Module(new GroundTestTraceGenerator()(p))
  case GeneratorKey => TrafficGeneratorParameters(
    maxRequests = 8192,
    startAddress = 0)
  case AddressBag => {
    val nSets = 2
    val nWays = 1
    val blockOffset = site(CacheBlockOffsetBits)
    val nBeats = site(CacheBlockBytes)/site(L1toL2Config).beatBytes
    List.tabulate(4 * nWays) { i =>
      Seq.tabulate(nBeats) { j => BigInt((j * 8) + ((i * nSets) << blockOffset)) }
    }.flatten
  }
})
