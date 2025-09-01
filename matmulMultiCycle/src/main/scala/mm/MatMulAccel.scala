package mm

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

class MatMulAccel(N: Int = 4, inW: Int = 8) extends Module {
  val accW = inW*2 + log2Ceil(N)
  val io = IO(new Bundle {
    val start = Input(Bool())
    val A = Input(Vec(N, Vec(N, UInt(inW.W))))
    val B = Input(Vec(N, Vec(N, UInt(inW.W))))
    val C = Output(Vec(N, Vec(N, UInt(accW.W))))
    val done = Output(Bool())
  })

  val Areg = Reg(Vec(N, Vec(N, UInt(inW.W))))
  val Breg = Reg(Vec(N, Vec(N, UInt(inW.W))))
  val Creg = RegInit(VecInit(Seq.fill(N)(VecInit(Seq.fill(N)(0.U(accW.W))))))
  io.C := Creg

  val i   = RegInit(0.U(log2Ceil(N).W))
  val j   = RegInit(0.U(log2Ceil(N).W))
  val k   = RegInit(0.U(log2Ceil(N).W))
  val sum = RegInit(0.U(accW.W))


  val cycles   = RegInit(0.U(32.W))
  val macs     = RegInit(0.U(32.W))
  val addsMath = RegInit(0.U(32.W))
  val printed  = RegInit(false.B)

  object S extends ChiselEnum { val Idle, Load, Mac, Write, Finish = Value }
  import S._
  val state = RegInit(Idle)
  io.done := (state === Finish)


  switch(state) {
    is(Idle) {
      when (io.start) {

        cycles   := 0.U
        macs     := 0.U
        addsMath := 0.U
        printed  := false.B
        state    := Load
      }
    }


    is(Load) {
      for (r <- 0 until N; c <- 0 until N) {
        Areg(r)(c) := io.A(r)(c)
        Breg(r)(c) := io.B(r)(c)
      }
      i := 0.U; j := 0.U; k := 0.U; sum := 0.U
      state := Mac
    }


    is(Mac) {
      sum := sum + (Areg(i)(k) * Breg(k)(j))
      macs   := macs + 1.U
      when (k =/= 0.U) {
        addsMath := addsMath + 1.U
      }
      when (k === (N-1).U) { state := Write } .otherwise { k := k + 1.U }
    }


    is(Write) {
      Creg(i)(j) := sum
      printf(p"C($i,$j) = $sum\n")

      sum := 0.U
      k   := 0.U
      when (j === (N-1).U) {
        j := 0.U
        when (i === (N-1).U) { state := Finish } .otherwise { i := i + 1.U; state := Mac }
      } .otherwise {
        j := j + 1.U
        state := Mac
      }
    }

    is(Finish) {
      when (!printed) {
        val expMacs    = (N*N*N).U
        val expAddsMth = (N*N*N - N*N).U
        val expCycles  = (1 + N*N + N*N*N).U

        printf(p"Total active cycles = $cycles (expected $expCycles)\n")
        printf(p"MACs executed       = $macs   (expected $expMacs)\n")
        printf(p"ADDs (math N-1/DP)  = $addsMath (expected $expAddsMth)\n")

        printed := true.B
      }
      when (!io.start) { state := Idle }
    }
  }

  when (state =/= S.Idle && state =/= S.Finish) {
    cycles := cycles + 1.U
  }
}