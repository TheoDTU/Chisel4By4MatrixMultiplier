// src/test/scala/matmul/BaseCase2x2Tester.scala
package matmul.tests

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import matmul.BaseCase2x2
import matmul.DataTypes._

class BaseCase2x2Tester extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "BaseCase2x2"

  def computeMatMul(a: Array[Array[Int]], b: Array[Array[Int]], n: Int): Array[Array[Int]] = {
    val result = Array.ofDim[Int](n, n)
    for (i <- 0 until n; j <- 0 until n) {
      var sum = 0
      for (k <- 0 until n) {
        sum += a(i)(k) * b(k)(j)
      }
      result(i)(j) = sum
    }
    result
  }

  it should "correctly multiply two 2x2 matrices using Naive method" in {
    val bitWidth = 16

    test(new BaseCase2x2(bitWidth, useStrassen = false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // input
      val matA = Array(
        Array(1, 2),
        Array(3, 4)
      )
      val matB = Array(
        Array(5, 6),
        Array(7, 8)
      )
      val expectedMatC = computeMatMul(matA, matB, 2)
      
      // test
      dut.io.in_a.valid.poke(false.B)
      dut.io.in_b.valid.poke(false.B)
      dut.clock.step(1)

      println("Driving Naive inputs...")
      dut.io.in_a.valid.poke(true.B)
      dut.io.in_b.valid.poke(true.B)
      for (i <- 0 until 2; j <- 0 until 2) {
        dut.io.in_a.bits.data(i)(j).poke(matA(i)(j).S(bitWidth.W))
        dut.io.in_b.bits.data(i)(j).poke(matB(i)(j).S(bitWidth.W))
      }
      
      dut.clock.step(1)
      dut.io.in_a.valid.poke(false.B)
      dut.io.in_b.valid.poke(false.B)

      println("Waiting for Naive pipeline to complete...")
      //wait for pipeline latency
      dut.clock.step(1) 
      
      println("Checking Naive output...")
      dut.io.out.valid.expect(true.B)
      for (i <- 0 until 2; j <- 0 until 2) {
        dut.io.out.bits.data(i)(j).expect(expectedMatC(i)(j).S(bitWidth.W))
      }
      dut.clock.step(1)
      dut.io.out.valid.expect(false.B)
    }
  }

  it should "correctly multiply two 2x2 matrices using Strassen method" in {
    val bitWidth = 16

    test(new BaseCase2x2(bitWidth, useStrassen = true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // input
      val matA = Array(
        Array(1, 2),
        Array(3, 4)
      )
      val matB = Array(
        Array(5, 6),
        Array(7, 8)
      )
      val expectedMatC = computeMatMul(matA, matB, 2)

      // test
      dut.io.in_a.valid.poke(false.B)
      dut.io.in_b.valid.poke(false.B)
      dut.clock.step(1)

      println("Driving Strassen inputs...")
      dut.io.in_a.valid.poke(true.B)
      dut.io.in_b.valid.poke(true.B)
      for (i <- 0 until 2; j <- 0 until 2) {
        dut.io.in_a.bits.data(i)(j).poke(matA(i)(j).S(bitWidth.W))
        dut.io.in_b.bits.data(i)(j).poke(matB(i)(j).S(bitWidth.W))
      }
      
      dut.clock.step(1)
      dut.io.in_a.valid.poke(false.B)
      dut.io.in_b.valid.poke(false.B)

      println("Waiting for Strassen pipeline to complete...")
      // wait for pipeline latency
      dut.clock.step(2)
      
      println("Checking Strassen output...")
      dut.io.out.valid.expect(true.B)
      for (i <- 0 until 2; j <- 0 until 2) {
        dut.io.out.bits.data(i)(j).expect(expectedMatC(i)(j).S(bitWidth.W))
      }
      dut.clock.step(1)
      dut.io.out.valid.expect(false.B)
      println("Simulation finished successfully!")
    }
  }
}