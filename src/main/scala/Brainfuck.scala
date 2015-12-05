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

}

class Brainfuck(val dentries: Int = 32768, val ientries: Int = 1024) extends Module {

  import Brainfuck._

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
      is(UInt(Inc)) {
        res := operand + UInt(1)
      }
      is(UInt(Dec)) {
        res := operand - UInt(1)
      }
      is(UInt(Get)) {
        res := Mux(io.rx.valid, io.rx.bits, operand)
      }
    }

    // write
    switch (op) {
      is(UInt(Inc), UInt(Dec), UInt(Get)) {
        data(dp) := res
      }
    }

    // update ip
    switch (op) {
      is(UInt(Jz)) {
        when (operand === UInt(0)) {
          ip := ip + UInt(1) + addr
        } .otherwise {
          ip := ip + UInt(2) // skip addr operand
        }
      }
      is(UInt(Jmp)) {
        ip := ip + UInt(1) - addr
      }
      is(UInt(Put)) {
        when (io.tx.ready) {
          ip := ip + UInt(1)
        }
      }
      is(UInt(Get)) {
        when (io.rx.valid) {
          ip := ip + UInt(1)
        }
      }
      is(UInt(PInc), UInt(PDec), UInt(Inc), UInt(Dec)) {
        ip := ip + UInt(1)
      }
    }

    // update dp
    switch (op) {
      is(UInt(PInc)) {
        dp := dp + UInt(1)
      }
      is(UInt(PDec)) {
        dp := dp - UInt(1)
      }
    }

    // IO
    switch (op) {
      is(UInt(Put)) {
        io.tx.valid := Bool(true)
        io.tx.bits := operand
      }
      is(UInt(Get)) {
        io.rx.ready := Bool(true)
      }
    }
  }
}

class BrainfuckTests(c: Brainfuck) extends Tester(c, isTrace = false) {

  def compile(str: String): Seq[Int] = {

    import Brainfuck._

    val bc = scala.collection.mutable.Buffer.empty[Seq[Int]]
    val ll = scala.collection.mutable.Stack.empty[Int]

    for (c <- str) {
      c match {
        case '+' => bc += Seq(Inc)
        case '-' => bc += Seq(Dec)
        case '>' => bc += Seq(PInc)
        case '<' => bc += Seq(PDec)
        case '.' => bc += Seq(Put)
        case ',' => bc += Seq(Get)
        case '[' => {
          ll.push(bc.length)
        }
        case ']' => {
          val i = ll.pop
          val j = bc.length + 1
          bc.insert(i, Seq(Jz,  j - i + 2))
          bc.insert(j, Seq(Jmp, j - i + 2))
        }
        case _ =>
      }
    }

    bc.flatten
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
