package matmul

import chisel3._
object DataTypes {
  
  // generic N x N Matrix Bundle
  class GenericMatrix(val n: Int, val bitWidth: Int) extends Bundle {
    val data = Vec(n, Vec(n, SInt(bitWidth.W)))
  }

  
  // A simple valid-only input interface.
  class ValidIO[T <: Bundle](gen: T) extends Bundle {
    val valid = Input(Bool())
    val bits = Input(gen.cloneType)
  }

  // simple valid-only output interface.
  class ValidOutIO[T <: Bundle](gen: T) extends Bundle {
    val valid = Output(Bool())
    val bits = Output(gen.cloneType)
  }

  // Bundle for the inputs
  class InputBundle(n: Int, bitWidth: Int) extends Bundle {
    val matA = new GenericMatrix(n, bitWidth)
    val matB = new GenericMatrix(n, bitWidth)
  }

  // Bundle for the data after the Splitters
  class SplitBundle(halfN: Int, bitWidth: Int) extends Bundle {
    val a, b, c, d, e, f, g, h = new GenericMatrix(halfN, bitWidth)
  }

  // Bundle for after the multiplication stage.
  class ProductBundle(numProducts: Int, halfN: Int, bitWidth: Int) extends Bundle {
    val products = Vec(numProducts, new GenericMatrix(halfN, bitWidth))
  }

  // Bundle for Strassen pre-computation
  class StrassenSBundle(bitWidth: Int) extends Bundle {
    val s = Vec(10, SInt(bitWidth.W))
    val m = Vec(4, SInt(bitWidth.W))
  }

  // Bundle for Strassen intermediate
  class StrassenPBundle(bitWidth: Int) extends Bundle {
    val p = Vec(7, SInt(bitWidth.W))
  }
}
