Jack Syntax Analyzer
=====================

Front end of the Jack compiler: tokenizes and parses Jack source files,
producing an XML representation of the program's syntactic structure.

Two-stage pipeline:

1. Tokenizer (JackTokenizer.java):
   - Strips comments (single-line // and block /* */) from the source.
   - Splits the remaining text into a flat token stream using a regex that
     treats keywords, symbols ({, }, (, ), [, ], ., ,, ;, operators),
     integer constants, string literals, and identifiers as token boundaries.
   - Classifies each token as KEYWORD, SYMBOL, INT_CONST, STRING_CONST, or
     IDENTIFIER.

2. Parser / Compilation Engine (CompilationEngine.java):
   - Implements a top-down recursive-descent parser that follows the Jack grammar.
   - Entry point is compileClass(); it recursively calls compileClassVarDec(),
     compileSubroutine(), compileStatements(), compileExpression(), etc.
   - Each grammar rule emits an opening XML tag, processes its sub-rules, then
     emits a closing tag — producing a complete parse tree in XML form.

For each input .jack file the analyzer produces a corresponding .xml file
whose tags mirror the Jack grammar (e.g. <class>, <subroutineDec>,
<statements>, <expression>).

Files:
- JackAnalyzer.java      — entry point; iterates over input files
- JackTokenizer.java     — lexer
- CompilationEngine.java — recursive-descent parser and XML emitter

Language: Java
