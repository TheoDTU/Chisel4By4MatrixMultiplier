// src/test/scala/matmul/StrassenStagesTester.scala
package matmul.tests

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import matmul.{StrassenPrecomputation, StrassenPostcomputation, StrassenMatMulPipelined}
import matmul.DataTypes._
import matmul.Strassen4x4ProductBundle

class StrassenStagesTester extends AnyFlatSpec with ChiselScalatestTester {

  val bitWidth = 16

  // input
  val matA = Array(
    Array(1, 2, 3, 4), Array(5, 6, 7, 8),
    Array(9, 10, 11, 12), Array(13, 14, 15, 16)
  )
  val matB = Array(
    Array(16, 15, 14, 13), Array(12, 11, 10, 9),
    Array(8, 7, 6, 5), Array(4, 3, 2, 1)
  )
  
  // Compute Expected Val
  def computeStrassenS(a: Array[Array[Int]], b: Array[Array[Int]], size: Int): Array[Array[Array[Int]]] = {
    val halfSize = size / 2
    val a11 = Array.ofDim[Int](halfSize, halfSize); val a12 = Array.ofDim[Int](halfSize, halfSize)
    val a21 = Array.ofDim[Int](halfSize, halfSize); val a22 = Array.ofDim[Int](halfSize, halfSize)
    val b11 = Array.ofDim[Int](halfSize, halfSize); val b12 = Array.ofDim[Int](halfSize, halfSize)
    val b21 = Array.ofDim[Int](halfSize, halfSize); val b22 = Array.ofDim[Int](halfSize, halfSize)
    
    for (i <- 0 until halfSize; j <- 0 until halfSize) {
      a11(i)(j) = a(i)(j); a12(i)(j) = a(i)(j + halfSize)
      a21(i)(j) = a(i + halfSize)(j); a22(i)(j) = a(i + halfSize)(j + halfSize)
      b11(i)(j) = b(i)(j); b12(i)(j) = b(i)(j + halfSize)
      b21(i)(j) = b(i + halfSize)(j); b22(i)(j) = b(i + halfSize)(j + halfSize)
    }

    def matAdd(m1: Array[Array[Int]], m2: Array[Array[Int]]): Array[Array[Int]] = {
      val res = Array.ofDim[Int](halfSize, halfSize)
      for (i <- 0 until halfSize; j <- 0 until halfSize) res(i)(j) = m1(i)(j) + m2(i)(j)
      res
    }
    def matSub(m1: Array[Array[Int]], m2: Array[Array[Int]]): Array[Array[Int]] = {
      val res = Array.ofDim[Int](halfSize, halfSize)
      for (i <- 0 until halfSize; j <- 0 until halfSize) res(i)(j) = m1(i)(j) - m2(i)(j)
      res
    }

    val s1 = matSub(b12, b22); val s2 = matAdd(a11, a12); val s3 = matAdd(a21, a22)
    val s4 = matSub(b21, b11); val s5 = matAdd(a11, a22); val s6 = matAdd(b11, b22)
    val s7 = matSub(a12, a22); val s8 = matAdd(b21, b22); val s9 = matSub(a11, a21)
    val s10 = matAdd(b11, b12)
    
    Array(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10)
  }

  def computeStrassenP(a: Array[Array[Int]], b: Array[Array[Int]], sMatrices: Array[Array[Array[Int]]]): Array[Array[Array[Int]]] = {
    val size = 4
    val halfSize = size / 2
    val a11 = Array.ofDim[Int](halfSize, halfSize); val a22 = Array.ofDim[Int](halfSize, halfSize)
    val b11 = Array.ofDim[Int](halfSize, halfSize); val b22 = Array.ofDim[Int](halfSize, halfSize)
    
    for (i <- 0 until halfSize; j <- 0 until halfSize) {
      a11(i)(j) = a(i)(j); a22(i)(j) = a(i + halfSize)(j + halfSize)
      b11(i)(j) = b(i)(j); b22(i)(j) = b(i + halfSize)(j + halfSize)
    }

    def matMul(m1: Array[Array[Int]], m2: Array[Array[Int]]): Array[Array[Int]] = {
      val res = Array.ofDim[Int](halfSize, halfSize)
      for (i <- 0 until halfSize; j <- 0 until halfSize) {
        var sum = 0
        for (k <- 0 until halfSize) {
          sum += m1(i)(k) * m2(k)(j)
        }
        res(i)(j) = sum
      }
      res
    }

    val p1 = matMul(a11, sMatrices(0))
    val p2 = matMul(sMatrices(1), b22)
    val p3 = matMul(sMatrices(2), b11)
    val p4 = matMul(a22, sMatrices(3))
    val p5 = matMul(sMatrices(4), sMatrices(5))
    val p6 = matMul(sMatrices(6), sMatrices(7))
    val p7 = matMul(sMatrices(8), sMatrices(9))
    
    Array(p1, p2, p3, p4, p5, p6, p7)
  }

  def computeStrassenC(pMatrices: Array[Array[Array[Int]]]): Array[Array[Int]] = {
    val size = 4
    val halfSize = size / 2
    val c = Array.ofDim[Int](size, size)

    def matAdd(m1: Array[Array[Int]], m2: Array[Array[Int]]): Array[Array[Int]] = {
      val res = Array.ofDim[Int](halfSize, halfSize)
      for (i <- 0 until halfSize; j <- 0 until halfSize) res(i)(j) = m1(i)(j) + m2(i)(j)
      res
    }
    def matSub(m1: Array[Array[Int]], m2: Array[Array[Int]]): Array[Array[Int]] = {
      val res = Array.ofDim[Int](halfSize, halfSize)
      for (i <- 0 until halfSize; j <- 0 until halfSize) res(i)(j) = m1(i)(j) - m2(i)(j)
      res
    }

    val p1 = pMatrices(0); val p2 = pMatrices(1); val p3 = pMatrices(2)
    val p4 = pMatrices(3); val p5 = pMatrices(4); val p6 = pMatrices(5); val p7 = pMatrices(6)

    val c11 = matAdd(matSub(matAdd(p5, p4), p2), p6)
    val c12 = matAdd(p1, p2)
    val c21 = matAdd(p3, p4)
    val c22 = matSub(matSub(matAdd(p5, p1), p3), p7)

    for (i <- 0 until halfSize; j <- 0 until halfSize) {
      c(i)(j) = c11(i)(j); c(i)(j + halfSize) = c12(i)(j)
      c(i + halfSize)(j) = c21(i)(j); c(i + halfSize)(j + halfSize) = c22(i)(j)
    }
    c
  }

  def pokeMatrix(port: GenericMatrix, values: Array[Array[Int]]): Unit = {
    for (i <- values.indices; j <- values(i).indices) {
      port.data(i)(j).poke(values(i)(j).S(bitWidth.W))
    }
  }

  def expectMatrix(port: GenericMatrix, values: Array[Array[Int]]): Unit = {
    for (i <- values.indices; j <- values(i).indices) {
      port.data(i)(j).expect(values(i)(j).S(bitWidth.W))
    }
  }

  // tests
  "StrassenPrecomputation Stage" should "produce the correct S matrices" in {
    test(new StrassenPrecomputation(bitWidth)) { dut =>
      val expectedS = computeStrassenS(matA, matB, 4)
      
      pokeMatrix(dut.io.in_a, matA)
      pokeMatrix(dut.io.in_b, matB)

      for (i <- expectedS.indices) {
        expectMatrix(dut.io.out.s(i), expectedS(i))
      }
    }
  }

  "StrassenPostcomputation Stage" should "produce the correct final matrix C from P matrices" in {
    test(new StrassenPostcomputation(bitWidth)) { dut =>
      val sMatrices = computeStrassenS(matA, matB, 4)
      val expectedP = computeStrassenP(matA, matB, sMatrices)
      val expectedC = computeStrassenC(expectedP)

      for(i <- expectedP.indices) {
        pokeMatrix(dut.io.in_p.p(i), expectedP(i))
      }
      expectMatrix(dut.io.out, expectedC)
    }
  }

  "Full StrassenMatMulPipelined" should "correctly multiply two 4x4 matrices" in {
    test(new StrassenMatMulPipelined(bitWidth)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // expceted
      val expectedMatC = {
        val matC = Array.ofDim[Int](4, 4)
        for (i <- 0 until 4; j <- 0 until 4) {
          var sum = 0
          for (k <- 0 until 4) {
            sum += matA(i)(k) * matB(k)(j)
          }
          matC(i)(j) = sum
        }
        matC
      }

      dut.io.a.valid.poke(false.B)
      dut.io.b.valid.poke(false.B)
      dut.clock.step(1)

      dut.io.a.valid.poke(true.B)
      dut.io.b.valid.poke(true.B)
      pokeMatrix(dut.io.a.bits, matA)
      pokeMatrix(dut.io.b.bits, matB)

      dut.clock.step(1)
      dut.io.a.valid.poke(false.B)
      dut.io.b.valid.poke(false.B)

      // wait for latency
      dut.clock.step(6)

      dut.io.c.valid.expect(true.B)
      expectMatrix(dut.io.c.bits, expectedMatC)

      dut.clock.step(1)
      dut.io.c.valid.expect(false.B)
    }
  }


}