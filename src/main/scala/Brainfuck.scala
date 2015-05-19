import Chisel._
import Uart._

class InitIO(aw: Int, dw: Int) extends Bundle {
  val valid = Bool(INPUT)
  val addr = UInt(INPUT, aw)
  val bits = UInt(INPUT, dw)
}

class UartIO extends Bundle {
  val tx = Decoupled(UInt(width = 8))
  val rx = Decoupled(UInt(width = 8)).flip
}

class Brainfuck extends Module {
  val dentries = 32768
  val ientries = 1024

  val io = new Bundle {
    val init = Bool(INPUT)
    val code = new InitIO(log2Up(ientries), 8)
    val data = new InitIO(log2Up(dentries), 8)
    val boot = Bool(INPUT)
    val uart = new UartIO
  }

  val code = Mem(UInt(width = 8), ientries)
  val data = Mem(UInt(width = 8), dentries)
  val ip = Reg(init = UInt(0, log2Up(ientries)))
  val dp = Reg(init = UInt(0, log2Up(dentries)))

  val op_inc :: op_dec :: op_pinc :: op_pdec :: op_put :: op_get :: op_jz :: op_jmp :: Nil = Enum(UInt(), 8)

  val next = UInt(width = 8)

  when (io.init) {
    when (io.code.valid) { code(io.code.addr) := io.code.bits }
    when (io.data.valid) { data(io.data.addr) := io.data.bits }
  }
  when (io.boot) {
    dp := UInt(0)
    ip := UInt(0)
  }

  next := UInt(0)

  io.uart.rx.ready := Bool(false)
  io.uart.tx.valid := Bool(false)
  io.uart.tx.bits := UInt(0)

  unless (io.init || io.boot) {
    val inst = code(ip)
    val addr = code(ip + UInt(1))

    val op = inst(2, 0)

    // read
    val operand = data(dp)

    // exec
    switch (op) {
      is(op_inc) {
        next := operand + UInt(1)
      }
      is(op_dec) {
        next := operand - UInt(1)
      }
    }

    // IO
    switch (op) {
      is(op_put) {
        when (io.uart.tx.ready) {
          io.uart.tx.valid := Bool(true)
          io.uart.tx.bits := operand
        }
      }
      is(op_get) {
        when (io.uart.rx.valid) {
          next := io.uart.rx.bits
          io.uart.rx.ready := Bool(false)
        } .otherwise {
          io.uart.rx.ready := Bool(true)
        }
      }
    }

    // write
    switch (op) {
      is(op_inc, op_dec, op_get) {
        data(dp) := next
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
        when (io.uart.tx.ready) {
          ip := ip + UInt(1)
        }
      }
      is(op_get) {
        when (io.uart.rx.valid) {
          ip := ip + UInt(1)
        }
      }
      is(op_pinc, op_pdec, op_inc, op_dec) {
        ip := ip + UInt(1)
      }
    }
  }
}

class BrainfuckTests(c: Brainfuck) extends Tester(c, isTrace = false) {

  import scala.collection.mutable.MutableList

  val helloworld = "+++++++++[>++++++++>+++++++++++>+++++<<<-]>.>++.+++++++..+++.>-.------------.<++++++++.--------.+++.------.--------.>+."

  def compile(str: String): (MutableList[UInt], Int) = {
    val bc = MutableList.empty[UInt]
    var i = 0
    while (i < str.length) {
      str(i) match {
        case '+' => bc += c.op_inc
        case '-' => bc += c.op_dec
        case '>' => bc += c.op_pinc
        case '<' => bc += c.op_pdec
        case '.' => bc += c.op_put
        case ',' => bc += c.op_get
        case '[' => {
          val (ibc, n) = compile(str.drop(i + 1))
          bc += c.op_jz
          bc += UInt(ibc.length + 3)
          bc ++= ibc
          bc += c.op_jmp
          bc += UInt(ibc.length + 3)
          i += n + 1
          assert(str(i) == ']')
        }
        case ']' => {
          return (bc, i)
        }
        case _ => assert(false)
      }
      i += 1
    }
    (bc, i)
  }

  val (bc, _) = compile(helloworld)

  poke(c.io.uart.tx.ready, 1)
  poke(c.io.uart.rx.valid, 0)
  poke(c.io.uart.rx.bits, 0)

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
    poke(c.io.code.bits, v.litValue())
  }

  step(1)

  poke(c.io.code.valid, 0)
  poke(c.io.init, 0)
  poke(c.io.boot, 1)

  step(1)

  poke(c.io.boot, 0)

  // main loop
  var s = ""
  for (t <- 0 until 1000) {
    step(1)

    if (peek(c.io.uart.tx.valid) != 0) {
      s += peek(c.io.uart.tx.bits).toChar
    }
  }

  println(s)

  assert(s == "Hello, world!")
}

object Brainfuck {
  def main(args: Array[String]): Unit = {
    chiselMainTest(args, () => Module(new Brainfuck)) { c =>
      new BrainfuckTests(c)
    }
  }
}
