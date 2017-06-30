// See LICENSE.SiFive for license details.

package freechips.rocketchip.groundtest

import Chisel._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.coreplex._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

import scala.math.max

case object TileId extends Field[Int]

class GroundTestCoreplex(implicit p: Parameters) extends BaseCoreplex {
  val tiles = List.tabulate(p(NTiles)) { i =>
    LazyModule(new GroundTestTile()(p.alter { (site, here, up) => {
      case TileId => i
      case CacheBlockOffsetBits => log2Up(site(CacheBlockBytes))
      case AmoAluOperandBits => site(XLen)
      case SharedMemoryTLEdge => tile_splitter.node.edgesIn(0)
    }}))
  }

  val fixer = LazyModule(new TLFIFOFixer)
  tile_splitter.node :=* fixer.node
  tiles.foreach { fixer.node :=* _.masterNode }

  val pbusRAM = LazyModule(new TLRAM(AddressSet(testRamAddr, 0xffff), false, pbusBeatBytes))
  pbusRAM.node := TLFragmenter(pbusBeatBytes, pbusBlockBytes)(pbus.node)

  override lazy val module = new GroundTestCoreplexModule(this, () => new GroundTestCoreplexBundle(this))
}

class GroundTestCoreplexBundle[+L <: GroundTestCoreplex](_outer: L) extends BaseCoreplexBundle(_outer) {
  val success = Bool(OUTPUT)
}

class GroundTestCoreplexModule[+L <: GroundTestCoreplex, +B <: GroundTestCoreplexBundle[L]](_outer: L, _io: () => B) extends BaseCoreplexModule(_outer, _io) {
  io.success := outer.tiles.map(_.module.io.success).reduce(_&&_)
}
