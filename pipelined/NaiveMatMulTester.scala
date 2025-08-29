package matmul.tests

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import matmul.NaiveMatMulPipelined


// NaiveMatMulPipelined test suite.
 
class NaiveMatMulTester extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "NaiveMatMulPipelined"

  it should "correctly multiply two 4x4 matrices" in {
    val bitWidth = 16

    test(new NaiveMatMulPipelined(bitWidth)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // input
      val matA = Array(
        Array(1, 2, 3, 4),
        Array(5, 6, 7, 8),
        Array(9, 10, 11, 12),
        Array(13, 14, 15, 16)
      )
      val matB = Array(
        Array(16, 15, 14, 13),
        Array(12, 11, 10, 9),
        Array(8, 7, 6, 5),
        Array(4, 3, 2, 1)
      )

      // expected result
      val expectedMatC = Array.ofDim[Int](4, 4)
      for (i <- 0 until 4; j <- 0 until 4) {
        var sum = 0
        for (k <- 0 until 4) {
          sum += matA(i)(k) * matB(k)(j)
        }
        expectedMatC(i)(j) = sum
      }
      
      // testing
      dut.io.a.valid.poke(false.B)
      dut.io.b.valid.poke(false.B)
      dut.clock.step(1)

      println("Driving inputs...")
      dut.io.a.valid.poke(true.B)
      dut.io.b.valid.poke(true.B)
      for (i <- 0 until 4; j <- 0 until 4) {
        dut.io.a.bits.data(i)(j).poke(matA(i)(j).S(bitWidth.W))
        dut.io.b.bits.data(i)(j).poke(matB(i)(j).S(bitWidth.W))
      }
      
      dut.clock.step(1)
      dut.io.a.valid.poke(false.B)
      dut.io.b.valid.poke(false.B)

      println("Waiting for pipeline to complete...")
      // wait for latency
      dut.clock.step(3) 
      
      println("Checking output...")
      dut.io.c.valid.expect(true.B)
      for (i <- 0 until 4; j <- 0 until 4) {
        dut.io.c.bits.data(i)(j).expect(expectedMatC(i)(j).S(bitWidth.W))
      }

      dut.clock.step(1)
      dut.io.c.valid.expect(false.B)
      
      println("Simulation finished successfully!")
    }
  }
}