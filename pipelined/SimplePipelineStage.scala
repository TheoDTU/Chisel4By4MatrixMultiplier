package matmul.components

import chisel3._
import chisel3.util.{ValidIO => _, _}
import matmul.DataTypes._

class SimplePipelineStage[T_IN <: Bundle, T_OUT <: Bundle](genIn: T_IN, genOut: T_OUT, logic: T_IN => T_OUT) extends Module {
  val io = IO(new Bundle {
    val in = new ValidIO(genIn.cloneType)
    val out = new ValidOutIO(genOut.cloneType)
  })
  val combinationalResult = logic(io.in.bits)
  val out_reg = RegInit(0.U.asTypeOf(genOut))
  when(io.in.valid) {
    out_reg := combinationalResult
  }
  io.out.bits := out_reg
  io.out.valid := RegNext(io.in.valid, false.B)
}
