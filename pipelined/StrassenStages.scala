package matmul

import chisel3._
import matmul.DataTypes._
import matmul.components._

// Helper bundle
class Strassen4x4PrecompBundle(bitWidth: Int) extends Bundle {
    val s = Vec(10, new GenericMatrix(2, bitWidth))
    val m = Vec(4, new GenericMatrix(2, bitWidth))
}

class Strassen4x4ProductBundle(bitWidth: Int) extends Bundle {
    val p = Vec(7, new GenericMatrix(2, bitWidth))
}


 // Stage 1 - input split and precomp
 
class StrassenPrecomputation(val bitWidth: Int) extends Module {
    val io = IO(new Bundle {
        val in_a = Input(new GenericMatrix(4, bitWidth))
        val in_b = Input(new GenericMatrix(4, bitWidth))
        val out = Output(new Strassen4x4PrecompBundle(bitWidth))
    })

    val splitterA = Module(new GenericSplitter(4, bitWidth)); splitterA.io.in := io.in_a
    val splitterB = Module(new GenericSplitter(4, bitWidth)); splitterB.io.in := io.in_b
    val (a,b,c,d) = (splitterA.io.outA, splitterA.io.outB, splitterA.io.outC, splitterA.io.outD)
    val (e,f,g,h) = (splitterB.io.outA, splitterB.io.outB, splitterB.io.outC, splitterB.io.outD)
    
    val adders = Seq.fill(6)(Module(new GenericMatrixAdder(2, bitWidth)))
    val subtractors = Seq.fill(4)(Module(new GenericMatrixSubtractor(2, bitWidth)))
    
    subtractors(0).io.a := f; subtractors(0).io.b := h; io.out.s(0) := subtractors(0).io.c
    adders(0).io.a      := a; adders(0).io.b      := b; io.out.s(1) := adders(0).io.c
    adders(1).io.a      := c; adders(1).io.b      := d; io.out.s(2) := adders(1).io.c
    subtractors(1).io.a := g; subtractors(1).io.b := e; io.out.s(3) := subtractors(1).io.c
    adders(2).io.a      := a; adders(2).io.b      := d; io.out.s(4) := adders(2).io.c
    adders(3).io.a      := e; adders(3).io.b      := h; io.out.s(5) := adders(3).io.c
    subtractors(2).io.a := b; subtractors(2).io.b := d; io.out.s(6) := subtractors(2).io.c
    adders(4).io.a      := g; adders(4).io.b      := h; io.out.s(7) := adders(4).io.c
    subtractors(3).io.a := a; subtractors(3).io.b := c; io.out.s(8) := subtractors(3).io.c
    adders(5).io.a      := e; adders(5).io.b      := f; io.out.s(9) := adders(5).io.c
    
    io.out.m(0) := a; io.out.m(1) := h; io.out.m(2) := e; io.out.m(3) := d
}


// Stage 2 - matrix mults
 
class StrassenProducts(val bitWidth: Int) extends Module {
    val io = IO(new Bundle {
        val in_valid = Input(Bool())
        val in_bundle = Input(new Strassen4x4PrecompBundle(bitWidth))
        val out_bundle = Output(new Strassen4x4ProductBundle(bitWidth))
        val out_valid = Output(Bool())
    })

    val mults = Seq.fill(7)(Module(new BaseCase2x2(bitWidth, useStrassen = true)))
    
    val subMatrixPairs = Seq(
        (io.in_bundle.m(0), io.in_bundle.s(0)), (io.in_bundle.s(1), io.in_bundle.m(1)), (io.in_bundle.s(2), io.in_bundle.m(2)),
        (io.in_bundle.m(3), io.in_bundle.s(3)), (io.in_bundle.s(4), io.in_bundle.s(5)), (io.in_bundle.s(6), io.in_bundle.s(7)),
        (io.in_bundle.s(8), io.in_bundle.s(9))
    )

    mults.zip(subMatrixPairs).zipWithIndex.foreach { case (((m, (matA, matB)), i)) =>
        m.io.in_a.valid := io.in_valid
        m.io.in_b.valid := io.in_valid
        m.io.in_a.bits := matA
        m.io.in_b.bits := matB
        io.out_bundle.p(i) := m.io.out.bits
    }
    io.out_valid := mults(0).io.out.valid
}


 // Stage 3 - postcomp
 
class StrassenPostcomputation(val bitWidth: Int) extends Module {
    val io = IO(new Bundle{
        val in_p = Input(new Strassen4x4ProductBundle(bitWidth))
        val out = Output(new GenericMatrix(4, bitWidth))
    })

    val merger = Module(new GenericMerger(4, bitWidth))
    val adds = Seq.fill(5)(Module(new GenericMatrixAdder(2, bitWidth)))
    val subs = Seq.fill(3)(Module(new GenericMatrixSubtractor(2, bitWidth)))
    val (p1,p2,p3,p4,p5,p6,p7) = (io.in_p.p(0), io.in_p.p(1), io.in_p.p(2), io.in_p.p(3), io.in_p.p(4), io.in_p.p(5), io.in_p.p(6))
    
    // C11 = P5 + P4 - P2 + P6
    val c11_t1 = Wire(new GenericMatrix(2, bitWidth)); adds(0).io.a:=p5; adds(0).io.b:=p4; c11_t1:=adds(0).io.c
    val c11_t2 = Wire(new GenericMatrix(2, bitWidth)); subs(0).io.a:=c11_t1; subs(0).io.b:=p2; c11_t2:=subs(0).io.c
    merger.io.inA := adds(1).io.c; adds(1).io.a:=c11_t2; adds(1).io.b:=p6

    // C12 = P1 + P2
    merger.io.inB := adds(2).io.c; adds(2).io.a:=p1; adds(2).io.b:=p2
    
    // C21 = P3 + P4
    merger.io.inC := adds(3).io.c; adds(3).io.a:=p3; adds(3).io.b:=p4
    
    // C22 = P1 + P5 - P3 - P7
    val c22_t1 = Wire(new GenericMatrix(2, bitWidth)); adds(4).io.a:=p1; adds(4).io.b:=p5; c22_t1:=adds(4).io.c
    val c22_t2 = Wire(new GenericMatrix(2, bitWidth)); subs(1).io.a:=c22_t1; subs(1).io.b:=p3; c22_t2:=subs(1).io.c
    merger.io.inD := subs(2).io.c; subs(2).io.a:=c22_t2; subs(2).io.b:=p7
    
    io.out := merger.io.out
}

