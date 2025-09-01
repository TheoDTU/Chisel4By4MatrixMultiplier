package mm

import chisel3._
import chiseltest._
import chiseltest.simulator.WriteVcdAnnotation
import org.scalatest.freespec.AnyFreeSpec

class MatMulAccelTest extends AnyFreeSpec with ChiselScalatestTester {
  val N   = 4
  val inW = 8

  private def dot(a: Array[Int], b: Array[Int]): Int = a.zip(b).map{ case (x,y) => x*y }.sum
  private def mm(a: Array[Array[Int]], b: Array[Array[Int]]): Array[Array[Int]] =
    Array.tabulate(N, N){ (i,j) => dot(a(i), Array.tabulate(N)(k => b(k)(j))) }

  private def printMatrix(name: String, m: Array[Array[Int]]): Unit = {
    println(s"$name =")
    m.foreach(row => println(row.mkString("[", " ", "]")))
  }

  private def readC(dut: MatMulAccel, N: Int): Array[Array[Int]] =
    Array.tabulate(N, N){ (r, c) => dut.io.C(r)(c).peek().litValue.toInt }

  "computes 4x4 matmul (random + fixed) and prints matrices" in {
    test(new MatMulAccel(N, inW))
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>

        def runOne(A: Array[Array[Int]], B: Array[Array[Int]]): Unit = {

          for (r <- 0 until N; col <- 0 until N) {
            c.io.A(r)(col).poke(A(r)(col).U)
            c.io.B(r)(col).poke(B(r)(col).U)
          }


          c.io.start.poke(true.B); c.clock.step(); c.io.start.poke(false.B)


          var guard = 0
          while (!c.io.done.peekBoolean() && guard < 2000) { c.clock.step(); guard += 1 }
          assert(guard < 2000, "Timeout waiting for done")


          val exp = mm(A,B)
          val got = readC(c, N)


          printMatrix("A", A)
          printMatrix("B", B)
          printMatrix("C_expected", exp)
          printMatrix("C_got", got)


          val expMacs    = N*N*N
          val expCycles  = 1 + N*N + expMacs
          println(s"Expected MACs   = $expMacs")
          println(s"Expected cycles = $expCycles  (Load + N^3 MACs + N^2 Writes)")


          for (r <- 0 until N; col <- 0 until N) {
            c.io.C(r)(col).expect(exp(r)(col).U, s"mismatch at ($r,$col)")
          }

          c.clock.step()
        }


        val A1 = Array(
          Array(1,2,3,4),
          Array(0,1,0,1),
          Array(5,0,0,0),
          Array(1,1,1,1)
        )
        val B1 = Array(
          Array(1,0,0,0),
          Array(0,1,0,0),
          Array(0,0,1,0),
          Array(0,0,0,1)
        )
        runOne(A1,B1)


        val rnd = new scala.util.Random(0)
        val A2 = Array.fill(N, N)(rnd.nextInt(10))
        val B2 = Array.fill(N, N)(rnd.nextInt(10))
        runOne(A2,B2)
      }
  }
}