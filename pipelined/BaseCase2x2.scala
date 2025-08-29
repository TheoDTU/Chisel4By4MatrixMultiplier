package matmul

import chisel3._
import chisel3.util.{ValidIO => _, _}
import matmul.DataTypes._
import matmul.components.SimplePipelineStage

/**
 * Pipelined 2x2 matrix multiplier for SInts.
 * 
 *
 * @param bitWidth The width of the SInt data elements.
 * @param useStrassen If true, generates the 3-stage Strassen pipeline. Otherwise, the 2-stage Naive pipeline.
 */
class BaseCase2x2(bitWidth: Int, useStrassen: Boolean) extends Module {
  val io = IO(new Bundle {
    val in_a = new ValidIO(new GenericMatrix(2, bitWidth))
    val in_b = new ValidIO(new GenericMatrix(2, bitWidth))
    val out  = new ValidOutIO(new GenericMatrix(2, bitWidth))
  })

  // input bundling
  val in_bundle = Wire(new InputBundle(2, bitWidth))
  in_bundle.matA := io.in_a.bits
  in_bundle.matB := io.in_b.bits

  val in_valid = io.in_a.valid && io.in_b.valid

  if (useStrassen) {
    // --- Strassen Pipeline

    // Stage 1 - Pre-computation
    val s_logic: InputBundle => StrassenSBundle = (in) => {
      val out = Wire(new StrassenSBundle(bitWidth))
      val (a,b,c,d) = (in.matA.data(0)(0), in.matA.data(0)(1), in.matA.data(1)(0), in.matA.data(1)(1))
      val (e,f,g,h) = (in.matB.data(0)(0), in.matB.data(0)(1), in.matB.data(1)(0), in.matB.data(1)(1))
      
      out.s(0) := f - h; 
      out.s(1) := a + b; 
      out.s(2) := c + d; 
      out.s(3) := g - e; 
      out.s(4) := a + d
      out.s(5) := e + h; 
      out.s(6) := b - d; 
      out.s(7) := g + h; 
      out.s(8) := a - c; 
      out.s(9) := e + f
      out.m(0) := a; out.m(1) := h; out.m(2) := e; out.m(3) := d
      out
    }
    val s_stage = Module(new SimplePipelineStage(in_bundle, new StrassenSBundle(bitWidth), s_logic))
    s_stage.io.in.valid := in_valid
    s_stage.io.in.bits  := in_bundle

    // Stage 2 - Multiplications
    val p_logic: StrassenSBundle => StrassenPBundle = (in) => {
      val out = Wire(new StrassenPBundle(bitWidth))
      out.p(0) := in.m(0) * in.s(0); 
      out.p(1) := in.s(1) * in.m(1); 
      out.p(2) := in.s(2) * in.m(2);
      out.p(3) := in.m(3) * in.s(3); 
      out.p(4) := in.s(4) * in.s(5); 
      out.p(5) := in.s(6) * in.s(7);
      out.p(6) := in.s(8) * in.s(9);
      out
    }
    val p_stage = Module(new SimplePipelineStage(new StrassenSBundle(bitWidth), new StrassenPBundle(bitWidth), p_logic))
    p_stage.io.in <> s_stage.io.out
    
    
    // Stage 3 - Final Additions
    val c_logic: StrassenPBundle => GenericMatrix = (in) => {
      val out = Wire(new GenericMatrix(2, bitWidth))
      out.data(0)(0) := in.p(4) + in.p(3) - in.p(1) + in.p(5)
      out.data(0)(1) := in.p(0) + in.p(1)
      out.data(1)(0) := in.p(2) + in.p(3)
      out.data(1)(1) := in.p(4) + in.p(0) - in.p(2) - in.p(6)
      out
    }
    val c_stage = Module(new SimplePipelineStage(new StrassenPBundle(bitWidth), new GenericMatrix(2, bitWidth), c_logic))
    c_stage.io.in <> p_stage.io.out
    
    io.out <> c_stage.io.out
  } else {
    // --- Naive Pipeline

    // Stage 1 - Multiplications
    val p_logic: InputBundle => ProductBundle = (in) => {
      val out = Wire(new ProductBundle(8, 1, bitWidth))
      val (a,b,c,d) = (in.matA.data(0)(0), in.matA.data(0)(1), in.matA.data(1)(0), in.matA.data(1)(1))
      val (e,f,g,h) = (in.matB.data(0)(0), in.matB.data(0)(1), in.matB.data(1)(0), in.matB.data(1)(1))
      out.products(0).data(0)(0) := a*e; out.products(1).data(0)(0) := b*g
      out.products(2).data(0)(0) := a*f; out.products(3).data(0)(0) := b*h
      out.products(4).data(0)(0) := c*e; out.products(5).data(0)(0) := d*g
      out.products(6).data(0)(0) := c*f; out.products(7).data(0)(0) := d*h
      out
    }
    val p_stage = Module(new SimplePipelineStage(in_bundle, new ProductBundle(8, 1, bitWidth), p_logic))
    p_stage.io.in.valid := in_valid
    p_stage.io.in.bits  := in_bundle

    // Stage 2 - Additions
    val c_logic: ProductBundle => GenericMatrix = (in) => {
      val out = Wire(new GenericMatrix(2, bitWidth))
      out.data(0)(0) := in.products(0).data(0)(0) + in.products(1).data(0)(0)
      out.data(0)(1) := in.products(2).data(0)(0) + in.products(3).data(0)(0)
      out.data(1)(0) := in.products(4).data(0)(0) + in.products(5).data(0)(0)
      out.data(1)(1) := in.products(6).data(0)(0) + in.products(7).data(0)(0)
      out
    }
    val c_stage = Module(new SimplePipelineStage(p_stage.io.out.bits, new GenericMatrix(2, bitWidth), c_logic))
    c_stage.io.in <> p_stage.io.out
    
    io.out <> c_stage.io.out
  }
}

