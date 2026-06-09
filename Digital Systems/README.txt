Digital Systems — Nand to Tetris
=================================

A full bottom-up construction of a working computer system, following the
Nand to Tetris curriculum. Starting from a single NAND gate primitive, each
project builds on the last until the system can run programs written in a
high-level language.

Build order:
1. SimpleLogicGates   — all basic logic gates built from NAND
2. ALU                — arithmetic/logic unit built from the gates above
3. RAM                — registers, RAM banks, and program counter
4. Hack-Computer-Architecture — full CPU and hardware platform
5. Hack-Assembly-Programs     — low-level programs in Hack assembly
6. Hack-Assembler     — assembler that translates Hack assembly to binary
7. VM-Translator-Arithmetic-and-Memory   — first half of a VM-to-assembly translator
8. VM-Translator-Control-and-Functions  — second half (branching and functions)
9. Jack-Syntax-Analyzer — front end of a compiler (tokenizer + parser)
10. Jack-Compiler       — full Jack-to-VM compiler
11. BlockShooter-Jack-Game — a game written in the Jack high-level language

Each subdirectory has a README.txt that describes its objective and design.
