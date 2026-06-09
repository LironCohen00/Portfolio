VM Translator — Arithmetic and Memory
=======================================

First half of a VM-to-Hack-assembly translator, handling stack arithmetic
and memory access commands from the Hack VM language.

The VM language is a stack-based intermediate language. All operations consume
operands from and push results onto a single global stack (SP points to the
next free slot in RAM).

Stack arithmetic translation:
- Binary ops (add, sub, and, or): pop two values, compute, push result.
  Translated to Hack assembly that decrements SP, reads the top two words,
  computes via the ALU, and writes back.
- Unary ops (neg, not): operate on the top-of-stack in place.
- Comparison ops (eq, gt, lt): pop two values, compare with D;JEQ/JGT/JLT,
  push -1 (true) or 0 (false). Jump addresses are computed from a running
  line counter maintained in CodeWriter.

Memory segment translation:
- local, argument, this, that: base address in LCL/ARG/THIS/THAT registers;
  push/pop uses base + offset.
- pointer: directly maps to THIS (pointer 0) and THAT (pointer 1).
- temp: maps to fixed RAM addresses 5–12.
- static: translated to file-scoped labels (FileName.i) in the assembly output.
- constant: push only; loads the constant value directly.

Files:
- VMTranslator.java — entry point; handles file I/O
- Parser.java       — tokenises VM commands into type, arg1, arg2
- CodeWriter.java   — emits Hack assembly for each VM command

Usage:
  java VMTranslator fileName.vm  →  produces fileName.asm in the same directory

Language: Java
