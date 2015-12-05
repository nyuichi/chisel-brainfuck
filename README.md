# chisel-brainfuck

Brainfuck CPU written in Chisel

## ISA

* Two pointers: ip, dp
* 8bit * 32768 register file
* 8bit * 1024 code rom
* An instruction may have one operand (variable-length encoding)

Instructions:

* `Inc`
* `Dec`
* `PInc`
* `PDec`
* `Put` (write to output port)
* `Get` (read from input port)
* `Jz addr` (jump to `ip + 1 + addr` if reg(dp) is set to zero)
* `Jmp addr` (always jump to `ip + 1 - addr`)

Simple Brainfuck compiler is included in the Tester class (see `compile` method in BrainfuckTests class). You can run your own brainfuck code on the generated cpu simulator by changing the value of `BrainfuckTests#helloworld`.
