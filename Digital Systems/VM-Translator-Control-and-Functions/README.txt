VM Translator — Control and Functions
=======================================

Full-scale VM-to-Hack-assembly translator, extending the arithmetic/memory
translator with branching and function-calling commands.

Branching translation:
- label <sym>: emits a Hack assembly label (functionName$sym).
- goto <sym>: unconditional jump via 0;JMP.
- if-goto <sym>: pops the top-of-stack; jumps if non-zero (D;JNE).

Function-calling convention translation:
- function f nVars: emits the function entry label and pushes nVars zeros
  onto the stack to initialise the function's local variables.
- call f nArgs: implements the VM calling convention —
    1. pushes the return address, saved LCL, ARG, THIS, THAT onto the stack
    2. sets ARG = SP - nArgs - 5 (repositions ARG for the callee)
    3. sets LCL = SP (new local segment base)
    4. jumps to the function entry label
    5. emits the return-address label so execution resumes here after return
- return: restores the caller's stack frame —
    1. copies the return value to ARG[0]
    2. restores SP, THAT, THIS, ARG, LCL from the saved frame
    3. jumps to the return address

Bootstrap code: the translator prepends assembly that sets SP=256 and
calls Sys.init, ensuring the VM program starts correctly.

Multi-file support: accepts a directory; concatenates all .vm files and emits
a single .asm output with all translated code.

Files:
- VMTranslator.java — entry point; handles single-file and directory input
- Parser.java       — tokenises VM commands into type, arg1, arg2
- CodeWriter.java   — emits Hack assembly for all VM command types

Usage:
  java VMTranslator fileName.vm   →  produces fileName.asm
  java VMTranslator dirName       →  produces dirName.asm from all .vm files in the directory

Language: Java
