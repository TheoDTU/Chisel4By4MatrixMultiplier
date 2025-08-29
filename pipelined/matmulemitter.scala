
package matmul

import java.io._
import chisel3._
import _root_.circt.stage.ChiselStage
import scopt.OptionParser

object MatMulGenerator extends App {
  // Configuration for the generator, simplified for the hardcoded modules.
  case class Config(
    algorithm: String = "naive",
    bitWidth: Int = 16
  )

  // scopt parser for command-line arguments.
  val parser = new OptionParser[Config]("MatMulGenerator") {
    head("A Pipelined 4x4 SInt Matrix Multiplier Hardware Generator")

    opt[String]("algo").action((x, c) => c.copy(algorithm = x))
      .text("Algorithm: 'naive' or 'strassen'. Default: naive")
    
    opt[Int]("bitWidth").action((x, c) => c.copy(bitWidth = x))
      .text("The width of the SInt data type. Default: 16")
  }
  
  parser.parse(args, Config()) match {
    case Some(config) =>
      println(s"Generating a 4x4 SInt-${config.bitWidth} multiplier using the ${config.algorithm} algorithm.")

      // Select DUT
      val dut = if (config.algorithm.toLowerCase == "naive") {
        () => new NaiveMatMulPipelined(config.bitWidth)
      } else if (config.algorithm.toLowerCase == "strassen") {
        () => new StrassenMatMulPipelined(config.bitWidth)
      } else {
        println(s"Error: Unknown algorithm '${config.algorithm}'. Please use 'naive' or 'strassen'.")
        sys.exit(1)
      }

      // Generate Verilog
      val filename = s"${config.algorithm}_4x4_sint${config.bitWidth}"
      new java.io.File("generated").mkdirs()
      
      val verilog = emitVerilog(
          dut(),
      )
      print(verilog)
      val pw = new PrintWriter(new File(s"generated/${filename}.v" ))
      pw.write(verilog)
      pw.close
      
      println(s"Successfully generated Verilog in generated/${filename}.sv")

    case None =>
      
  }
}