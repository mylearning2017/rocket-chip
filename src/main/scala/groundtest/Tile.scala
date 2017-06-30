// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.groundtest

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.coreplex._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.chip._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.ParameterizedBundle
import scala.collection.mutable.ListBuffer

case object BuildGroundTest extends Field[Parameters => GroundTest]

case class GroundTestTileParams(
    uncached: Int = 0,
    ptw: Int = 0,
    maxXacts: Int = 1,
    dcache: Option[DCacheParams] = Some(DCacheParams())) extends TileParams {
  val icache = None
  val btb = None
  val rocc = Nil
  val core = RocketCoreParams(nPMPs = 0) //TODO remove this
  val cached = if(dcache.isDefined) 1 else 0
  val dataScratchpadBytes = 0
}
case object GroundTestKey extends Field[Seq[GroundTestTileParams]]

trait HasGroundTestConstants {
  val timeoutCodeBits = 4
  val errorCodeBits = 4
}

trait HasGroundTestParameters {
  implicit val p: Parameters
  val tileParams = p(GroundTestKey)(p(TileId))
  val nUncached = tileParams.uncached
  val nCached = tileParams.cached
  val nPTW = tileParams.ptw
  val memStart = p(ExtMem).base
  val memStartBlock = memStart >> log2Up(p(CacheBlockBytes))
}

class GroundTestStatus extends Bundle with HasGroundTestConstants {
  val finished = Bool(OUTPUT)
  val timeout = Valid(UInt(width = timeoutCodeBits))
  val error = Valid(UInt(width = errorCodeBits))
}

class GroundTestIO(implicit val p: Parameters) extends ParameterizedBundle()(p)
    with HasGroundTestParameters {
  val cache = Vec(nCached, new HellaCacheIO)
  val mem = Vec(nUncached, new ClientUncachedTileLinkIO)
  val ptw = Vec(nPTW, new TLBPTWIO)
  val status = new GroundTestStatus
}

abstract class GroundTest(implicit val p: Parameters) extends Module
    with HasGroundTestParameters {
  val io = new GroundTestIO
}

class GroundTestTile(implicit p: Parameters) extends LazyModule
    with HasGroundTestParameters {
  val slave = None
  val dcacheOpt = tileParams.dcache.map { dc => HellaCache(0, dc.nMSHRs == 0) }
  val ucLegacy = LazyModule(new TLLegacy)

   val masterNode = TLOutputNode()
   dcacheOpt.foreach { masterNode := _.node }
   masterNode := TLHintHandler()(ucLegacy.node)

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val out = masterNode.bundleOut
      val success = Bool(OUTPUT)
    }

    val test = p(BuildGroundTest)(p)

    val ptwPorts = ListBuffer.empty ++= test.io.ptw
    val uncachedArbPorts = ListBuffer.empty ++= test.io.mem

    dcacheOpt foreach { dcache =>
      val dcacheArb = Module(new HellaCacheArbiter(nCached))

      dcacheArb.io.requestor.zip(test.io.cache).foreach {
        case (requestor, cache) =>
          val dcacheIF = Module(new SimpleHellaCacheIF())
          dcacheIF.io.requestor <> cache
          requestor <> dcacheIF.io.cache
      }
      dcache.module.io.cpu <> dcacheArb.io.mem

      // SimpleHellaCacheIF leaves invalidate_lr dangling, so we wire it to false
      dcache.module.io.cpu.invalidate_lr := Bool(false)

      ptwPorts += dcache.module.io.ptw
    }

    if (ptwPorts.size > 0) {
      val ptw = Module(new DummyPTW(ptwPorts.size))
      ptw.io.requestors <> ptwPorts
    }

    if (uncachedArbPorts.isEmpty) {
      ucLegacy.module.io.legacy.acquire.valid := Bool(false)
      ucLegacy.module.io.legacy.grant.ready := Bool(true)
    } else {
      val uncachedArb = Module(new ClientUncachedTileLinkIOArbiter(uncachedArbPorts.size))
      uncachedArb.io.in <> uncachedArbPorts
      ucLegacy.module.io.legacy <> uncachedArb.io.out
    }

    io.success := test.io.status.finished
  }
}
