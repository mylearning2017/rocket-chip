// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.groundtest

import Chisel._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.rocket.HellaCacheIO
import freechips.rocketchip.util._

import scala.util.Random

case class TrafficGeneratorParameters(
  maxRequests: Int,
  startAddress: BigInt)
case object GeneratorKey extends Field[TrafficGeneratorParameters]

trait HasTrafficGeneratorParameters extends HasGroundTestParameters {
  implicit val p: Parameters

  val genParams = p(GeneratorKey)
  val nGens = p(GroundTestKey).map(
    cs => cs.uncached + cs.cached).reduce(_ + _)
  val genTimeout = 8192
  val maxRequests = genParams.maxRequests
  val startAddress = genParams.startAddress

  val genWordBits = 32
  val genWordBytes = genWordBits / 8
  val wordOffset = log2Ceil(genWordBytes)
  val wordSize = UInt(log2Ceil(genWordBytes))

  require(startAddress % BigInt(genWordBytes) == 0)
}

class HellaCacheGenerator(id: Int)
    (implicit val p: Parameters) extends Module with HasTrafficGeneratorParameters {
  val io = new Bundle {
    val mem = new HellaCacheIO
    val status = new GroundTestStatus
  }

  val timeout = SimpleTimer(genTimeout, io.mem.req.fire(), io.mem.resp.valid)
  assert(!timeout, s"Cached generator ${id} timed out waiting for response")
  io.status.timeout.valid := timeout
  io.status.timeout.bits := UInt(id)

  val (s_start :: s_write :: s_read :: s_finished :: Nil) = Enum(Bits(), 4)
  val state = Reg(init = s_start)
  val sending = Reg(init = Bool(false))

  val (req_cnt, req_wrap) = Counter(io.mem.resp.valid, maxRequests)

  val part_of_req_addr =
    if (log2Ceil(nGens) > 0) {
      Cat(UInt(id, log2Ceil(nGens)),
          UInt(0, wordOffset))
    } else {
      UInt(0, wordOffset)
    }
  val req_addr = UInt(startAddress) + Cat(req_cnt, part_of_req_addr)
  val req_data = Cat(UInt(id, log2Up(nGens)), req_cnt, part_of_req_addr)

  io.mem.req.valid := sending && !io.status.finished
  io.mem.req.bits.addr := req_addr
  io.mem.req.bits.data := req_data
  io.mem.req.bits.typ  := wordSize
  io.mem.req.bits.cmd  := Mux(state === s_write, M_XWR, M_XRD)
  io.mem.req.bits.tag  := UInt(0)

  when (state === s_start) { sending := Bool(true); state := s_write }

  when (io.mem.req.fire()) { sending := Bool(false) }
  when (io.mem.resp.valid) { sending := Bool(true) }

  when (req_wrap) { state := Mux(state === s_write, s_read, s_finished) }

  io.status.finished := (state === s_finished)

  def data_match(recv: Bits, expected: Bits): Bool = {
    val recv_resized = Wire(Bits(width = genWordBits))
    val exp_resized = Wire(Bits(width = genWordBits))

    recv_resized := recv
    exp_resized := expected
    recv_resized === exp_resized
  }

  val data_mismatch = io.mem.resp.valid && io.mem.resp.bits.has_data &&
    !data_match(io.mem.resp.bits.data, req_data)

  io.status.error.valid := data_mismatch
  io.status.error.bits := UInt(id)

  assert(!data_mismatch,
    s"Received incorrect data in cached generator ${id}")
}

class GeneratorTest(implicit p: Parameters)
    extends GroundTest()(p) with HasTrafficGeneratorParameters {

  val idStart = p(GroundTestKey).take(p(TileId))
    .map(settings => settings.cached + settings.uncached)
    .foldLeft(0)(_ + _)

  val cached = List.tabulate(nCached) { i =>
    val realId = idStart + i
    Module(new HellaCacheGenerator(realId))
  }
  io.cache <> cached.map(_.io.mem)

  //val uncached = List.tabulate(nUncached) { i =>
  //  val realId = idStart + nCached + i
  //  Module(new UncachedTileLinkGenerator(realId))
  //}
  //io.mem <> uncached.map(_.io.mem)

  val gen_debug = cached.map(_.io.status) ++ uncached.map(_.io.status)
  io.status := DebugCombiner(gen_debug)
}

object DebugCombiner {
  def apply(debugs: Seq[GroundTestStatus]): GroundTestStatus = {
    val out = Wire(new GroundTestStatus)
    out.finished := debugs.map(_.finished).reduce(_ && _)
    out.timeout  := ValidMux(debugs.map(_.timeout))
    out.error    := ValidMux(debugs.map(_.error))
    out
  }
}
