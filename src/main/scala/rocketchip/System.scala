// See LICENSE.SiFive for license details.

package freechips.rocketchip.chip

import Chisel._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyMultiIOModuleImp}
import freechips.rocketchip.util.ElaborationArtefacts

/** BareSystem is the root class for creating a top-level RTL module */
abstract class BareSystem(implicit p: Parameters) extends LazyModule {
  ElaborationArtefacts.add("graphml", graphML)
}

abstract class BareSystemModule[+L <: BareSystem](_outer: L) extends LazyMultiIOModuleImp(_outer) {
  val outer = _outer
}

/** Base System class with no peripheral devices or ports added */
abstract class BaseSystem(implicit p: Parameters) extends BareSystem with HasSystemNetworks {
  override val module: BaseSystemModule[BaseSystem]
}

abstract class BaseSystemModule[+L <: BaseSystem](_outer: L) extends BareSystemModule(_outer)
