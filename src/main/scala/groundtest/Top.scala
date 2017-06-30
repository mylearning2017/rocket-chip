// See LICENSE.SiFive for license details.

package freechips.rocketchip.groundtest

import Chisel._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.chip._

class GroundTestTop(implicit p: Parameters) extends BaseSystem
    with HasPeripheryMasterAXI4MemPort
    with HasPeripheryTestRAMSlave {
  override lazy val module = new GroundTestTopModule(this)

  val coreplex = LazyModule(new GroundTestCoreplex)

  socBus.node := coreplex.mmio
  coreplex.mmioInt := intBus.intnode
  (mem zip coreplex.mem) foreach { case (xbar, channel) => xbar.node :=* channel }
}

class GroundTestTopModule[+L <: GroundTestTop](_outer: L) extends BaseSystemModule(_outer)
    with HasPeripheryMasterAXI4MemPortModuleImp {
  val io_success = IO(Bool(OUTPUT))
  io_success := outer.coreplex.module.io.success
}
