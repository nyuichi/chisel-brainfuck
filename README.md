# chisel-brainfuck

Brainfuck CPU written in Chisel

## ISA

* Two pointers: ip, dp
* 8bit * 32768 register file
* 8bit * 1024 code rom
* An instruction may have one operand (variable-length encoding)

Instructions:

* `op_inc`
* `op_dec`
* `op_pinc`
* `op_pdec`
* `op_put` (write to output port)
* `op_get` (read from input port)
* `op_jz addr` (jump to `ip + 1 + addr` if reg(dp) is set to zero)
* `op_jmp addr` (always jump to `ip + 1 - addr`)

Tester class also contains a simple brainfuck compiler. You can test your own brainfuck code run on the generated cpu simulator by changing the value of `BrainfuckTests#helloworld`.
