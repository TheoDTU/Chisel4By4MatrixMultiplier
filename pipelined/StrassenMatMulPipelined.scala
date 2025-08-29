package matmul

import chisel3._
import matmul.DataTypes._
import matmul.components._

/**
 *  4x4 matrix multiplier using Strassen's algorithm.
 * 
 * @param bitWidth The width of the SInt.
 */
class StrassenMatMulPipelined(val bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val a = new ValidIO(new GenericMatrix(4, bitWidth))
    val b = new ValidIO(new GenericMatrix(4, bitWidth))
    val c = new ValidOutIO(new GenericMatrix(4, bitWidth))
  })

  // Stage 1 - Input
  val inputBundle = Wire(new InputBundle(4, bitWidth))
  inputBundle.matA := io.a.bits
  inputBundle.matB := io.b.bits
  
  val inputStage = Module(new SimplePipelineStage(inputBundle, inputBundle, (in: InputBundle) => in))
  inputStage.io.in.valid := io.a.valid && io.b.valid
  inputStage.io.in.bits  := inputBundle

  // Stage 2 - Precomp
  val precomp = Module(new StrassenPrecomputation(bitWidth))
  precomp.io.in_a := inputStage.io.out.bits.matA
  precomp.io.in_b := inputStage.io.out.bits.matB
  
  val precompStage = Module(new SimplePipelineStage(precomp.io.out, precomp.io.out, (in: Strassen4x4PrecompBundle) => in))
  precompStage.io.in.valid := inputStage.io.out.valid
  precompStage.io.in.bits := precomp.io.out

  // Stage 3 - Products
  val products = Module(new StrassenProducts(bitWidth))
  products.io.in_valid := precompStage.io.out.valid
  products.io.in_bundle := precompStage.io.out.bits

  val productsStage = Module(new SimplePipelineStage(products.io.out_bundle, products.io.out_bundle, (in: Strassen4x4ProductBundle) => in))
  productsStage.io.in.valid := products.io.out_valid
  productsStage.io.in.bits := products.io.out_bundle

  // Stage 4 - Postcomp
  val postcomp = Module(new StrassenPostcomputation(bitWidth))
  postcomp.io.in_p := productsStage.io.out.bits

  val postcompStage = Module(new SimplePipelineStage(postcomp.io.out, postcomp.io.out, (in: GenericMatrix) => in))
  postcompStage.io.in.valid := productsStage.io.out.valid
  postcompStage.io.in.bits := postcomp.io.out

  io.c.bits := postcompStage.io.out.bits
  io.c.valid := postcompStage.io.out.valid
}
