package matmul.components.tests

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import matmul.components.{GenericMatrixAdder, GenericMatrixSubtractor, GenericSplitter, GenericMerger}
import matmul.DataTypes.GenericMatrix

// Generic Matrix Components.
 
class MatrixComponentsTester extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "GenericMatrixAdder"

  it should "correctly add two 2x2 matrices" in {
    val n = 2
    val bitWidth = 16
    test(new GenericMatrixAdder(n, bitWidth)) { dut =>
      val a = Array(Array(1, 2), Array(3, 4))
      val b = Array(Array(5, 6), Array(7, 8))
      val expected = Array(Array(6, 8), Array(10, 12))

      for (i <- 0 until n; j <- 0 until n) {
        dut.io.a.data(i)(j).poke(a(i)(j).S)
        dut.io.b.data(i)(j).poke(b(i)(j).S)
      }

      dut.clock.step(1)

      for (i <- 0 until n; j <- 0 until n) {
        dut.io.c.data(i)(j).expect(expected(i)(j).S)
      }
    }
  }

  // GenericMatrixSubtractor

  behavior of "GenericMatrixSubtractor"

  it should "correctly subtract two 2x2 matrices" in {
    val n = 2
    val bitWidth = 16
    test(new GenericMatrixSubtractor(n, bitWidth)) { dut =>
      val a = Array(Array(10, 20), Array(30, 40))
      val b = Array(Array(1, 2), Array(3, 4))
      val expected = Array(Array(9, 18), Array(27, 36))

      for (i <- 0 until n; j <- 0 until n) {
        dut.io.a.data(i)(j).poke(a(i)(j).S)
        dut.io.b.data(i)(j).poke(b(i)(j).S)
      }

      dut.clock.step(1)

      for (i <- 0 until n; j <- 0 until n) {
        dut.io.c.data(i)(j).expect(expected(i)(j).S)
      }
    }
  }

  // GenericSplitter

  behavior of "GenericSplitter"

  it should "correctly split a 4x4 matrix into four 2x2 sub-matrices" in {
    val n = 4
    val bitWidth = 16
    test(new GenericSplitter(n, bitWidth)) { dut =>
      val fullMatrix = Array(
        Array(1, 2, 3, 4),
        Array(5, 6, 7, 8),
        Array(9, 10, 11, 12),
        Array(13, 14, 15, 16)
      )

      val expectedA = Array(Array(1, 2), Array(5, 6))
      val expectedB = Array(Array(3, 4), Array(7, 8))
      val expectedC = Array(Array(9, 10), Array(13, 14))
      val expectedD = Array(Array(11, 12), Array(15, 16))

      for (i <- 0 until n; j <- 0 until n) {
        dut.io.in.data(i)(j).poke(fullMatrix(i)(j).S)
      }

      dut.clock.step(1)

      val halfN = n / 2
      for (i <- 0 until halfN; j <- 0 until halfN) {
        dut.io.outA.data(i)(j).expect(expectedA(i)(j).S)
        dut.io.outB.data(i)(j).expect(expectedB(i)(j).S)
        dut.io.outC.data(i)(j).expect(expectedC(i)(j).S)
        dut.io.outD.data(i)(j).expect(expectedD(i)(j).S)
      }
    }
  }

  // GenericMerger

  behavior of "GenericMerger"

  it should "correctly merge four 2x2 matrices into a 4x4 matrix" in {
    val n = 4
    val bitWidth = 16
    test(new GenericMerger(n, bitWidth)) { dut =>
      val inA = Array(Array(1, 2), Array(5, 6))
      val inB = Array(Array(3, 4), Array(7, 8))
      val inC = Array(Array(9, 10), Array(13, 14))
      val inD = Array(Array(11, 12), Array(15, 16))

      val expectedMerged = Array(
        Array(1, 2, 3, 4),
        Array(5, 6, 7, 8),
        Array(9, 10, 11, 12),
        Array(13, 14, 15, 16)
      )

      val halfN = n / 2
      for (i <- 0 until halfN; j <- 0 until halfN) {
        dut.io.inA.data(i)(j).poke(inA(i)(j).S)
        dut.io.inB.data(i)(j).poke(inB(i)(j).S)
        dut.io.inC.data(i)(j).poke(inC(i)(j).S)
        dut.io.inD.data(i)(j).poke(inD(i)(j).S)
      }

      dut.clock.step(1)

      for (i <- 0 until n; j <- 0 until n) {
        dut.io.out.data(i)(j).expect(expectedMerged(i)(j).S)
      }
    }
  }
}
