import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class CompilationEngine {
    FileWriter fileWriter;
    JackTokenizer tokenizer;
    int indentCount;

    public CompilationEngine(File inputFile, File outputFile) throws IOException {
        tokenizer = new JackTokenizer(inputFile);
        fileWriter = new FileWriter(outputFile);
        indentCount = 0;
    }
    public void compileClass() throws IOException {
        fileWriter.write("<class>\n");
        indentCount += 1;
        writeKeyword();
        tokenizer.advance();
        writeIdentifier();
        tokenizer.advance();
        writeSymbol();
        tokenizer.advance();
        while ((tokenizer.keyWord().equals("static")) || (tokenizer.keyWord().equals("field"))) {
            compileClassVarDec();
        }
        while ((tokenizer.keyWord().equals("constructor")) || (tokenizer.keyWord().equals("function")) || (tokenizer.keyWord().equals("method"))) {
            compileSubroutine();
        }
        writeSymbol();
        indentCount -= 1;
        fileWriter.write("</class>");
        fileWriter.close();
    }

    public void compileSubroutine() throws IOException {
        indent();
        fileWriter.write(("<subroutineDec>\n"));
        indentCount += 1;
        writeKeyword();
        tokenizer.advance();
        if (tokenizer.tokenType().equals("KEYWORD")) {
            writeKeyword();
        } else {
            writeIdentifier();
        }
        tokenizer.advance();
        writeIdentifier();
        tokenizer.advance();
        writeSymbol();
        indent();
        fileWriter.write(("<parameterList>\n"));
        indentCount += 1;
        compileParameterList();
        indentCount -= 1;
        indent();
        fileWriter.write(("</parameterList>\n"));
        writeSymbol();
        compileSubroutineBody();
        tokenizer.advance();
        indentCount -= 1;
        indent();
        fileWriter.write(("</subroutineDec>\n"));
    }
    public void compileParameterList() throws IOException {
        tokenizer.advance();
        if (!tokenizer.tokenType().equals("SYMBOL")) {
            if (tokenizer.tokenType().equals("KEYWORD")) {
                writeKeyword();
            } else {
                writeIdentifier();
            }
            tokenizer.advance();
            writeIdentifier();
            tokenizer.advance();
            if (tokenizer.symbol() == ',') {
                writeSymbol();
                compileParameterList();
            } 
        }
    }

    public void compileSubroutineBody() throws IOException {
        indent();
        fileWriter.write(("<subroutineBody>\n"));
        indentCount += 1;
        tokenizer.advance();
        writeSymbol();
        tokenizer.advance();
        while (tokenizer.keyWord().equals("var")) {
            compileVarDec();
        } 
        compileStatements();
        writeSymbol();
        indentCount -= 1;
        indent();
        fileWriter.write(("</subroutineBody>\n"));
    }

    public void compileClassVarDec() throws IOException {
        indent();
        fileWriter.write(("<classVarDec>\n"));
        indentCount += 1;
        writeKeyword();
        compileTypeAndVarName();
        writeSymbol();
        indentCount -= 1;
        indent();
        fileWriter.write(("</classVarDec>\n"));
        tokenizer.advance();
    }

    public void compileTypeAndVarName() throws IOException {
        tokenizer.advance();
        if (tokenizer.tokenType().equals("KEYWORD")) {
            writeKeyword();
        } else {
            writeIdentifier();
        }
        tokenizer.advance();
        writeIdentifier();
        tokenizer.advance();
        while (tokenizer.symbol() == ',') {
            writeSymbol();
            tokenizer.advance();
            writeIdentifier();
            tokenizer.advance();
        }
    }

    public void compileVarDec() throws IOException {
        indent();
        fileWriter.write(("<varDec>\n"));
        indentCount += 1;
        writeKeyword();
        compileTypeAndVarName();
        writeSymbol();
        tokenizer.advance();
        indentCount -= 1;
        indent();
        fileWriter.write(("</varDec>\n"));
    }

    public void compileStatements() throws IOException {
        indent();
        fileWriter.write(("<statements>\n"));
        indentCount += 1;
        while (tokenizer.tokenType().equals("KEYWORD")) {
            if (tokenizer.keyWord().equals("let")) {
                compileLet();
            } else if (tokenizer.keyWord().equals("if")) {
                compileIf();
            } else if (tokenizer.keyWord().equals("while")) {
                compileWhile();
            }else if (tokenizer.keyWord().equals("do")) {
                compileDo();
            }else if (tokenizer.keyWord().equals("return")) {
                compileReturn();
            }
        }
        indentCount -= 1;
        indent();
        fileWriter.write(("</statements>\n"));
    }

    public void compileLet() throws IOException {
        indent();
        fileWriter.write(("<letStatement>\n"));
        indentCount += 1;
        writeKeyword();
        tokenizer.advance();
        writeIdentifier();
        tokenizer.advance();
        if (tokenizer.tokenType().equals("SYMBOL") && (tokenizer.symbol() == '[')) {
            writeSymbol();
            tokenizer.advance();
            compileExpression();
            writeSymbol();
            tokenizer.advance();
        }
        writeSymbol();
        tokenizer.advance();
        compileExpression();
        writeSymbol();
        tokenizer.advance();
        indentCount -= 1;
        indent();
        fileWriter.write(("</letStatement>\n"));
    }

    public void compileIf() throws IOException {
        indent();
        fileWriter.write(("<ifStatement>\n"));
        indentCount += 1;
        writeKeyword();
        tokenizer.advance();
        writeSymbol();
        tokenizer.advance();
        compileExpression();
        writeSymbol();
        tokenizer.advance();
        writeSymbol();
        tokenizer.advance();
        compileStatements();
        writeSymbol();
        tokenizer.advance();
        if (tokenizer.keyWord().equals("else")) {
            writeKeyword();
            tokenizer.advance();
            writeSymbol();
            tokenizer.advance();
            compileStatements();
            writeSymbol();
            tokenizer.advance();
        }
        indentCount -= 1;
        indent();
        fileWriter.write(("</ifStatement>\n"));
    }

    public void compileWhile() throws IOException {
        indent();
        fileWriter.write(("<whileStatement>\n"));
        indentCount += 1;
        writeKeyword();
        tokenizer.advance();
        writeSymbol();
        tokenizer.advance();
        compileExpression();
        writeSymbol();
        tokenizer.advance();
        writeSymbol();
        tokenizer.advance();
        compileStatements();
        writeSymbol();
        tokenizer.advance();
        indentCount -= 1;
        indent();
        fileWriter.write(("</whileStatement>\n"));
    }

    public void compileDo() throws IOException {
        indent();
        fileWriter.write(("<doStatement>\n"));
        indentCount += 1;
        writeKeyword();
        tokenizer.advance();
        compileSubroutineCall();
        tokenizer.advance();
        writeSymbol();
        tokenizer.advance();
        indentCount -= 1;
        indent();
        fileWriter.write(("</doStatement>\n"));
    }

    public void compileSubroutineCall() throws IOException {
        writeIdentifier(); // className|SubroutineName
        tokenizer.advance();
        writeSymbol(); // "(" | "."
        tokenizer.advance();
        if (tokenizer.tokenType().equals("IDENTIFIER")) { // if the case was className.varName
            writeIdentifier(); // subroutineName
            tokenizer.advance();
            writeSymbol(); // "("
            tokenizer.advance();
        }
        compileExpressionList();
        writeSymbol();
    }

    public void compileReturn() throws IOException {
        indent();
        fileWriter.write(("<returnStatement>\n"));
        indentCount += 1;
        writeKeyword();
        tokenizer.advance();
        if (!tokenizer.tokenType().equals("SYMBOL") || tokenizer.symbol() != ';') {
        compileExpression();
        }
        writeSymbol();
        tokenizer.advance();
        indentCount -= 1;
        indent();
        fileWriter.write(("</returnStatement>\n"));
    }

    public void compileExpression() throws IOException {
        indent();
        fileWriter.write(("<expression>\n"));
        indentCount += 1;
        compileTerm();
        while ((tokenizer.symbol() == '+') || (tokenizer.symbol() == '-') || (tokenizer.symbol() == '*') || (tokenizer.symbol() == '/')
        || (tokenizer.symbol() == '&') || (tokenizer.symbol() == '|') || (tokenizer.symbol() == '<') || (tokenizer.symbol() == '>') || (tokenizer.symbol() == '=')) {
                writeSymbol();
                tokenizer.advance();
                compileTerm();
        }
        indentCount -= 1;
        indent();
        fileWriter.write(("</expression>\n"));
    }

    public void compileTerm() throws IOException {
        indent();
        fileWriter.write(("<term>\n"));
        indentCount += 1;
        switch (tokenizer.tokenType()) {
            case "INT_CONST":
                writeIntConstant();
                tokenizer.advance();
                break;
            case "STRING_CONST":
                writeStringConstant();
                tokenizer.advance();
                break;
            case "KEYWORD":
                writeKeyword();
                tokenizer.advance();
                break;
            case "SYMBOL":
                if (tokenizer.symbol() == '(') {
                    writeSymbol();
                    tokenizer.advance();
                    compileExpression();
                    writeSymbol();
                    tokenizer.advance();
                }else { // unaryOp
                    writeSymbol();
                    tokenizer.advance();
                    compileTerm();
                }
                break;
            case "IDENTIFIER":
                writeIdentifier();
                tokenizer.advance();
                if ((tokenizer.tokenType().equals("SYMBOL")) && (tokenizer.symbol() == '[')) {
                    writeSymbol();
                    tokenizer.advance();
                    compileExpression();
                    writeSymbol();
                    tokenizer.advance();
                } else if ((tokenizer.tokenType().equals("SYMBOL")) && (tokenizer.symbol() == '(')) { // HERE IS THE PROBLEM
                    writeSymbol();
                    tokenizer.advance();
                    compileExpressionList();
                    writeSymbol();
                    tokenizer.advance();
                } else if (tokenizer.symbol() == '.') {
                    writeSymbol(); // .
                    tokenizer.advance();
                    writeIdentifier(); // subroutine name
                    tokenizer.advance();
                    writeSymbol(); // (
                    tokenizer.advance();
                    compileExpressionList();
                    writeSymbol(); // )
                    tokenizer.advance();
                }
                break;
        }
        indentCount -= 1;
        indent();
        fileWriter.write(("</term>\n"));
    }

    public int compileExpressionList() throws IOException {
        indent();
        fileWriter.write(("<expressionList>\n"));
        indentCount += 1;
        if (tokenizer.tokenType().equals("SYMBOL") && (tokenizer.symbol() == ')')) {
            indentCount -= 1;
            indent();
            fileWriter.write(("</expressionList>\n"));
            return 0;
        }
        compileExpression();
        int num = 1;
        while (tokenizer.symbol() == ',') {
            writeSymbol();
            tokenizer.advance();
            compileExpression();
            num++;
        }
        indentCount -= 1;
        indent();
        fileWriter.write(("</expressionList>\n"));
        return num;
    }

    public void writeIdentifier() throws IOException  {
        String temp = "<identifier> " + tokenizer.identifier() + " </identifier>\n";
        indent();
        fileWriter.write(temp);
    }
    public void writeSymbol() throws IOException {
        String temp;
        if (tokenizer.symbol() == '>') {
            temp = "<symbol> &gt; </symbol>\n";
        }
        else if (tokenizer.symbol() == '<') {
            temp = "<symbol> &lt; </symbol>\n";
        }
        else if (tokenizer.symbol() == '&') {
            temp = "<symbol> &amp; </symbol>\n";
        } else {
            temp = "<symbol> " + tokenizer.symbol() + " </symbol>\n";
        }
        indent();
        fileWriter.write(temp);
    }
    public void writeKeyword() throws IOException {
        String temp = "<keyword> " + tokenizer.keyWord() + " </keyword>\n";
        indent();
        fileWriter.write(temp);
    }
    public void writeIntConstant() throws IOException {
        String temp = "<integerConstant> " + tokenizer.intVal() + " </integerConstant>\n";
        indent();
        fileWriter.write(temp);
    }
    public void writeStringConstant() throws IOException {
        String temp = "<stringConstant> " + tokenizer.stringVal() + " </stringConstant>\n";
        indent();
        fileWriter.write(temp);
    }

    public void indent() throws IOException {
        for (int i = 0; i < indentCount; i++) {
            fileWriter.write("\t");
        }
    }
}
    


