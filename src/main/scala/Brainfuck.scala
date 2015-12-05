import Chisel._
import Uart._

class InitIO(aw: Int, dw: Int) extends Bundle {
  val valid = Bool(INPUT)
  val addr = UInt(INPUT, aw)
  val bits = UInt(INPUT, dw)
}

object Brainfuck {

  val Inc  = 0
  val Dec  = 1
  val PInc = 2
  val PDec = 3
  val Put  = 4
  val Get  = 5
  val Jz   = 6
  val Jmp  = 7

  lazy val InstList = Seq(Inc, Dec, PInc, PDec, Put, Get, Jz, Jmp)

}

class Brainfuck(val dentries: Int = 32768, val ientries: Int = 1024) extends Module {

  val io = new Bundle {
    val init = Bool(INPUT)
    val code = new InitIO(log2Up(ientries), 8)
    val data = new InitIO(log2Up(dentries), 8)
    val boot = Bool(INPUT)
    val tx = Decoupled(UInt(width = 8))
    val rx = Decoupled(UInt(width = 8)).flip
  }

  val code = Mem(UInt(width = 8), ientries)
  val data = Mem(UInt(width = 8), dentries)
  val ip = Reg(init = UInt(0, log2Up(ientries)))
  val dp = Reg(init = UInt(0, log2Up(dentries)))

  val Seq(op_inc, op_dec, op_pinc, op_pdec, op_put, op_get, op_jz, op_jmp) = Brainfuck.InstList map { UInt(_) }

  val res = UInt(width = 8)

  when (io.init) {
    when (io.code.valid) { code(io.code.addr) := io.code.bits }
    when (io.data.valid) { data(io.data.addr) := io.data.bits }
  }
  when (io.boot) {
    dp := UInt(0)
    ip := UInt(0)
  }

  res := UInt(0)

  io.rx.ready := Bool(false)
  io.tx.valid := Bool(false)
  io.tx.bits := UInt(0)

  unless (io.init || io.boot) {
    val inst = code(ip)
    val addr = code(ip + UInt(1))

    val op = inst(2, 0)

    // read
    val operand = data(dp)

    // exec
    switch (op) {
      is(op_inc) {
        res := operand + UInt(1)
      }
      is(op_dec) {
        res := operand - UInt(1)
      }
      is(op_get) {
        res := Mux(io.rx.valid, io.rx.bits, operand)
      }
    }

    // write
    switch (op) {
      is(op_inc, op_dec, op_get) {
        data(dp) := res
      }
    }

    // update ip
    switch (op) {
      is(op_jz) {
        when (operand === UInt(0)) {
          ip := ip + UInt(1) + addr
        } .otherwise {
          ip := ip + UInt(2) // skip addr operand
        }
      }
      is(op_jmp) {
        ip := ip + UInt(1) - addr
      }
      is(op_put) {
        when (io.tx.ready) {
          ip := ip + UInt(1)
        }
      }
      is(op_get) {
        when (io.rx.valid) {
          ip := ip + UInt(1)
        }
      }
      is(op_pinc, op_pdec, op_inc, op_dec) {
        ip := ip + UInt(1)
      }
    }

    // update dp
    switch (op) {
      is(op_pinc) {
        dp := dp + UInt(1)
      }
      is(op_pdec) {
        dp := dp - UInt(1)
      }
    }

    // IO
    switch (op) {
      is(op_put) {
        when (io.tx.ready) {
          io.tx.valid := Bool(true)
          io.tx.bits := operand
        }
      }
      is(op_get) {
        when (io.rx.valid) {
          io.rx.ready := Bool(false)
        } .otherwise {
          io.rx.ready := Bool(true)
        }
      }
    }
  }
}

class BrainfuckTests(c: Brainfuck) extends Tester(c, isTrace = false) {

  import Brainfuck._

  def compile(str: String): Seq[Int] = {
    var i = 0
    def go(): Seq[Int] = {
      val bc = scala.collection.mutable.MutableList.empty[Int]
      while (i < str.length) {
        str(i) match {
          case '+' => i += 1; bc += Inc
          case '-' => i += 1; bc += Dec
          case '>' => i += 1; bc += PInc
          case '<' => i += 1; bc += PDec
          case '.' => i += 1; bc += Put
          case ',' => i += 1; bc += Get
          case '[' => {
            i += 1
            val ibc = go()
            bc += Jz
            bc += ibc.length + 3
            bc ++= ibc
            assert(str(i) == ']')
            i += 1
            bc += Jmp
            bc += ibc.length + 3
          }
          case ']' => {
            return bc
          }
          case _ => i += 1
        }
      }
      bc
    }
    go()
  }

  def boot(bc: Seq[Int]) {
    poke(c.io.tx.ready, 0)
    poke(c.io.rx.valid, 0)
    poke(c.io.rx.bits, 0)

    poke(c.io.init, 1)
    poke(c.io.data.valid, 0)
    poke(c.io.code.valid, 0)

    // init data
    for (t <- 0 until 32768) {
      step(1)

      poke(c.io.data.valid, 1)
      poke(c.io.data.addr, t)
      poke(c.io.data.bits, 0)
    }

    step(1)

    poke(c.io.data.valid, 0)

    // init code
    for ((v, t) <- bc.zipWithIndex) {
      step(1)

      poke(c.io.code.valid, 1)
      poke(c.io.code.addr, t)
      poke(c.io.code.bits, v)
    }

    step(1)

    poke(c.io.code.valid, 0)
    poke(c.io.init, 0)
    poke(c.io.boot, 1)

    step(1)

    poke(c.io.boot, 0)
  }

  def run(time: Int): String = {
    poke(c.io.tx.ready, 1)

    var s = ""
    for (t <- 0 until time) {
      step(1)

      if (peek(c.io.tx.valid) != 0) {
        s += peek(c.io.tx.bits).toChar
      }
    }
    s
  }

  val bc = compile("helloworld:+++++++++[>++++++++>+++++++++++>+++++<<<-]>.>++.+++++++..+++.>-.------------.<++++++++.--------.+++.------.--------.>+.")

  boot(bc)

  val s = run(1000)

  println(s)

  assert(s == "Hello, world!")
}

object BrainfuckMain {
  def main(args: Array[String]): Unit = {
    chiselMainTest(args, () => Module(new Brainfuck)) { c =>
      new BrainfuckTests(c)
    }
  }
}
