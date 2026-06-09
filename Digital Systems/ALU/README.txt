ALU — Arithmetic Logic Unit
============================

Implementation of the Hack computer's ALU and its supporting arithmetic chips
in HDL, building up from binary addition to a full-featured ALU.

Design:
- HalfAdder: adds two 1-bit inputs; outputs sum and carry. Built from Xor (sum)
  and And (carry).
- FullAdder: adds three 1-bit inputs (a, b, carry-in); outputs sum and carry-out.
  Built from two HalfAdders.
- Add16: ripple-carry 16-bit adder built by chaining 16 FullAdders.
- Inc16: adds 1 to a 16-bit value; implemented as Add16 with a constant 1 input.
- ALU: the complete Hack ALU. Takes two 16-bit inputs (x, y) and six 1-bit
  control bits (zx, nx, zy, ny, f, no) that select the operation:
    • zx/nx: zero or negate the x input pre-computation
    • zy/ny: zero or negate the y input pre-computation
    • f: select Add16 (f=1) or And16 (f=0) as the operation
    • no: negate the output post-computation
  This encoding lets the same chip compute add, sub, and, or, not, and more.
  Outputs: 16-bit result, plus zr (result is zero) and ng (result is negative).

Language: HDL (Hack Hardware Description Language)
