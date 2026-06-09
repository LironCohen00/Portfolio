Hack Assembler
==============

A two-pass assembler that translates Hack assembly (.asm) into 16-bit binary
machine code (.hack) for the Hack hardware platform.

Implementation:

Pass 1 — symbol collection:
  - Scans the source file without emitting output.
  - Each label declaration (e.g. (LOOP)) is added to the symbol table
    with its ROM address (the line count of actual instructions seen so far).
  - Pre-defined symbols (R0–R15, SP, LCL, ARG, THIS, THAT, SCREEN, KBD) are
    seeded into the table before the first pass begins.

Pass 2 — code generation:
  - A-instructions (@value or @symbol): if the operand is a number, encode it
    directly as a 15-bit binary value. If it's a new variable symbol, allocate
    it to the next available RAM address (starting at 16) and emit that address.
  - C-instructions (dest=comp;jump): the parser splits the instruction into
    its three fields and the Code module looks up each field's bit encoding
    from fixed tables; the result is the bit string 111accccccdddjjj.

Files:
- HackAssembler.java — entry point; orchestrates the two passes
- Parser.java        — tokenises lines into instruction type, symbol, dest, comp, jump
- Code.java          — maps symbolic fields to their binary encodings
- SymbolTable.java   — hash-map backed symbol table

Usage:
  java HackAssembler fileName.asm  →  produces fileName.hack in the same directory

Language: Java
