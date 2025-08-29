package matmul.components

import chisel3._
import matmul.DataTypes.GenericMatrix


// Generic Matrix adder
class GenericMatrixAdder(n: Int, bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(new GenericMatrix(n, bitWidth))
    val b = Input(new GenericMatrix(n, bitWidth))
    val c = Output(new GenericMatrix(n, bitWidth))
  })
  for (i <- 0 until n; j <- 0 until n) {
    io.c.data(i)(j) := io.a.data(i)(j) + io.b.data(i)(j)
  }
}


// Generic Matrix subtractor
class GenericMatrixSubtractor(n: Int, bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(new GenericMatrix(n, bitWidth))
    val b = Input(new GenericMatrix(n, bitWidth))
    val c = Output(new GenericMatrix(n, bitWidth))
  })
  for (i <- 0 until n; j <- 0 until n) {
    io.c.data(i)(j) := io.a.data(i)(j) - io.b.data(i)(j)
  }
}


// Splits matrix into submatrices
class GenericSplitter(n: Int, bitWidth: Int) extends Module {
  val halfN = n / 2
  val io = IO(new Bundle {
    val in = Input(new GenericMatrix(n, bitWidth))
    val outA, outB, outC, outD = Output(new GenericMatrix(halfN, bitWidth))
  })
  for (i <- 0 until halfN; j <- 0 until halfN) {
    io.outA.data(i)(j) := io.in.data(i)(j)
    io.outB.data(i)(j) := io.in.data(i)(j + halfN)
    io.outC.data(i)(j) := io.in.data(i + halfN)(j)
    io.outD.data(i)(j) := io.in.data(i + halfN)(j + halfN)
  }
}


// Merges four matrices into one matrix.
class GenericMerger(n: Int, bitWidth: Int) extends Module {
  val halfN = n / 2
  val io = IO(new Bundle {
    val inA, inB, inC, inD = Input(new GenericMatrix(halfN, bitWidth))
    val out = Output(new GenericMatrix(n, bitWidth))
  })
  for (i <- 0 until halfN; j <- 0 until n / 2) {
    io.out.data(i)(j)               := io.inA.data(i)(j)
    io.out.data(i)(j + halfN)       := io.inB.data(i)(j)
    io.out.data(i + halfN)(j)       := io.inC.data(i)(j)
    io.out.data(i + halfN)(j + halfN) := io.inD.data(i)(j)
  }
}
