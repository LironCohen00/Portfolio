Simple Logic Gates
==================

Implementation of all foundational logic gates using HDL (Hardware Description
Language), with NAND as the only primitive.

Every gate is built compositionally — NAND is given; all others are derived
from it. For example, Not is a NAND with both inputs tied together; And is a
Not applied to a NAND; Or follows from De Morgan's law; Xor combines And/Or/Not.
The 16-bit variants apply the corresponding 1-bit gate to each bit in parallel.
Multiplexors and demultiplexors use select lines to route signals.

Chip Name    Description
---------    -----------
Nand         NAND gate (primitive — all other gates derive from this)
Not          NOT gate
And          AND gate
Or           OR gate
Xor          XOR gate
Mux          2-way multiplexor — outputs a or b based on select bit
DMux         Demultiplexor — routes input to one of two outputs
Not16        16-bit NOT
And16        16-bit AND
Or16         16-bit OR
Mux16        16-bit 2-way multiplexor
Or8Way       8-input OR: Or(in0, in1, ..., in7)
Mux4Way16    16-bit 4-way multiplexor (2 select bits)
Mux8Way16    16-bit 8-way multiplexor (3 select bits)
DMux4Way     4-way demultiplexor
DMux8Way     8-way demultiplexor

Language: HDL (Hack Hardware Description Language)
