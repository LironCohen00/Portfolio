Jack Compiler
==============

A full Jack-to-VM compiler that translates Jack source code (.jack) into
Hack VM bytecode (.vm), which then runs on the VM translator built in previous
projects.

Pipeline:

1. Tokenizer (JackTokenizer.java):
   - Lexes Jack source into a token stream (keywords, symbols, identifiers,
     integer and string constants), stripping all comments.

2. Symbol Table (SymbolTable.java / Symbol.java):
   - Maintains two scopes: class-level (field, static) and subroutine-level
     (argument, local).
   - Each symbol stores its name, type, kind, and a running index per kind —
     this index directly maps to VM segment offsets (e.g. local 0, argument 1).
   - The subroutine-level table is reset at the start of each new subroutine.

3. Compilation Engine (CompilationEngine.java):
   - Recursive-descent parser that drives code generation in a single pass.
   - Statements: let → pop to variable, if/while → VM branching with
     auto-generated unique labels, do → function call + pop temp 0, return.
   - Expressions: evaluated left-to-right; operators are emitted after both
     operands (postfix VM form); unary operators use neg/not.
   - Object construction: constructor allocates heap memory via Memory.alloc,
     sets the this pointer; method calls push the receiver object as argument 0.

4. VM Writer (VMWriter.java):
   - Thin abstraction that writes formatted VM commands to the output file
     (push, pop, arithmetic, label, goto, if-goto, function, call, return).

Files:
- JackCompiler.java      — entry point; iterates over .jack files
- JackTokenizer.java     — lexer
- CompilationEngine.java — recursive-descent parser and code generator
- SymbolTable.java       — two-scope symbol table
- Symbol.java            — symbol entity (name, type, kind, index)
- VMWriter.java          — VM command emitter

Language: Java
