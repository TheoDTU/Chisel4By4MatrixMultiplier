package matmul

import chisel3._
import matmul.DataTypes._
import matmul.components._

/**
 * pipelined 4x4 matrix multiplier
 *
 * @param bitWidth The width of the SInt data elements.
 */
class NaiveMatMulPipelined(val bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val a = new ValidIO(new GenericMatrix(4, bitWidth))
    val b = new ValidIO(new GenericMatrix(4, bitWidth))
    val c = new ValidOutIO(new GenericMatrix(4, bitWidth))
  })

  // Stage 0 - Register inputs
  val inputBundle = Wire(new InputBundle(4, bitWidth))
  inputBundle.matA := io.a.bits
  inputBundle.matB := io.b.bits
  
  val inputStage = Module(new SimplePipelineStage(inputBundle, inputBundle, (in: InputBundle) => in))
  inputStage.io.in.valid := io.a.valid && io.b.valid
  inputStage.io.in.bits  := inputBundle

  // Instantiate 4x4
  val core = Module(new NaiveMatMul4x4Core(bitWidth))
  
  core.io.in_a.valid := inputStage.io.out.valid
  core.io.in_b.valid := inputStage.io.out.valid
  core.io.in_a.bits := inputStage.io.out.bits.matA
  core.io.in_b.bits := inputStage.io.out.bits.matB
  
  io.c.bits := core.io.out.bits
  io.c.valid := core.io.out.valid
}



class NaiveMatMul4x4Core(val bitWidth: Int) extends Module {
    val io = IO(new Bundle {
        val in_a = new ValidIO(new GenericMatrix(4, bitWidth))
        val in_b = new ValidIO(new GenericMatrix(4, bitWidth))
        val out = new ValidOutIO(new GenericMatrix(4, bitWidth))
    })

    // 2x2 multipliers
    val mults = Seq.fill(8)(Module(new BaseCase2x2(bitWidth, useStrassen = false)))

    // Stage 1 - Split and Distribute
    val splitterA = Module(new GenericSplitter(4, bitWidth)); splitterA.io.in := io.in_a.bits
    val splitterB = Module(new GenericSplitter(4, bitWidth)); splitterB.io.in := io.in_b.bits
    val (a,b,c,d) = (splitterA.io.outA, splitterA.io.outB, splitterA.io.outC, splitterA.io.outD)
    val (e,f,g,h) = (splitterB.io.outA, splitterB.io.outB, splitterB.io.outC, splitterB.io.outD)
    
    val subMatrixPairs = Seq((a,e), (b,g), (a,f), (b,h), (c,e), (d,g), (c,f), (d,h))
    
    mults.zip(subMatrixPairs).foreach { case (m, (matA, matB)) =>
        m.io.in_a.valid := io.in_a.valid; m.io.in_b.valid := io.in_b.valid
        m.io.in_a.bits := matA; m.io.in_b.bits := matB
    }

    // Stage 2 - Postcomp
    val postCompStage_in_bits = Wire(new ProductBundle(8, 2, bitWidth))
    postCompStage_in_bits.products := VecInit(mults.map(_.io.out.bits))

    val postCompLogic: ProductBundle => GenericMatrix = (in) => {
      val out = Wire(new GenericMatrix(4, bitWidth))
      val merger = Module(new GenericMerger(4, bitWidth))
      val adders = Seq.fill(4)(Module(new GenericMatrixAdder(2, bitWidth)))
      
      adders(0).io.a:=in.products(0); adders(0).io.b:=in.products(1); merger.io.inA:=adders(0).io.c
      adders(1).io.a:=in.products(2); adders(1).io.b:=in.products(3); merger.io.inB:=adders(1).io.c
      adders(2).io.a:=in.products(4); adders(2).io.b:=in.products(5); merger.io.inC:=adders(2).io.c
      adders(3).io.a:=in.products(6); adders(3).io.b:=in.products(7); merger.io.inD:=adders(3).io.c
      
      out := merger.io.out
      out
    }
    
    val postCompStage = Module(new SimplePipelineStage(postCompStage_in_bits, new GenericMatrix(4, bitWidth), postCompLogic))
    postCompStage.io.in.valid := mults(0).io.out.valid
    postCompStage.io.in.bits  := postCompStage_in_bits
    
    io.out <> postCompStage.io.out
}