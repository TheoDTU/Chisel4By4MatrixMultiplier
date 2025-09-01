package mm
import chisel3.stage.ChiselStage

object Emit extends App {
  (new ChiselStage).emitVerilog(
    new MatMulAccel(N = 4, inW = 8),
      Array("--target-dir", "generated_mm")
  )
}