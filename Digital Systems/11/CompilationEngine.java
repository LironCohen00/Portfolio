import java.io.File;
import java.io.IOException;
import java.util.Arrays;

import javax.swing.plaf.synth.SynthPasswordFieldUI;


public class CompilationEngine {
    JackTokenizer tokenizer;
    VMWriter vmWriter;
    SymbolTable symbolTable;
    String currentClass;
    String currentSubroutine;
    String SubroutineType;
    int labelIndex;
    static Character[] operators = {'+', '-', '/', '*', '<', '>', '=' , '&' , '|'};
    StackTraceElement[] stackTraceElements;

    public CompilationEngine(File inputFile, File ouptutFile) throws IOException {
        tokenizer = new JackTokenizer(inputFile);
        vmWriter = new VMWriter(ouptutFile);
        symbolTable = new SymbolTable();
        labelIndex = 0;
        stackTraceElements = Thread.currentThread().getStackTrace();
        compileClass();
    }

    public void compileClass() throws IOException {
        tokenizer.advance();
        currentClass = tokenizer.identifier();
        tokenizer.advance();
        tokenizer.advance();
        compileClassVarDec();
        while ((!tokenizer.tokenType().equals("SYMBOL")) || ((tokenizer.tokenType().equals("SYMBOL")) && (tokenizer.symbol() != '}'))) {
            compileSubroutine();
        }
        vmWriter.close();
    }

    public void compileClassVarDec() {
        // check if there even is any variable declarations (split into 2 different if statements for clarity)
        if (tokenizer.tokenType().equals("SYMBOL") && (tokenizer.symbol() == '}')) {
            return;
        }
        if ( tokenizer.tokenType().equals("KEYWORD") && (tokenizer.keyWord().equals("function") || tokenizer.keyWord().equals("constructor") || tokenizer.keyWord().equals("method")) ) {
            return;
        }
        Symbol.KIND kind = null;
        switch (tokenizer.keyWord()) {
            case ("field"):
                kind = Symbol.KIND.FIELD;
                break;
            case ("static"):
                kind = Symbol.KIND.STATIC;
                break;
        }
        tokenizer.advance();
        String type = getType();
        tokenizer.advance();
        boolean DoneAllVar = false;
        String name;
        do {
            name = tokenizer.identifier();
            symbolTable.define(name, type, kind);
            tokenizer.advance();
            if (tokenizer.tokenType().equals("SYMBOL") && (tokenizer.symbol() == ';')) { // if its a ;
                DoneAllVar = true;
            }      
            tokenizer.advance();
        } while (DoneAllVar == false);
        compileClassVarDec();
    }

    public String getType() {
        if (tokenizer.tokenType().equals("KEYWORD")){
            return tokenizer.keyWord();
        }
        return tokenizer.identifier();
    }

    public void compileSubroutine() throws IOException {
        symbolTable.startSubroutine();
        SubroutineType = tokenizer.keyWord();
        tokenizer.advance();
        tokenizer.advance();
        currentSubroutine = tokenizer.identifier();
        tokenizer.advance();
        tokenizer.advance();
        compileParameterList();
        tokenizer.advance();
        tokenizer.advance();
        compileVarDec();
        vmWriter.writeFunction(currentClass + "." + currentSubroutine,  symbolTable.varCount(Symbol.KIND.VAR));
        compileSubroutineBody();
    }

    public void compileParameterList() {
        if (tokenizer.tokenType().equals("SYMBOL") && (tokenizer.symbol() == ')')) {
            return;
        }
        do {
            String type = getType();
            tokenizer.advance();
            String name = tokenizer.identifier();
            symbolTable.define(name, type, Symbol.KIND.ARG);
            tokenizer.advance();
        } while (tokenizer.symbol() != ')');
    }

    public void compileVarDec() {
        // check if there even is any variable declarations (split into 2 different if statements for clarity)
        if (!tokenizer.tokenType().equals("KEYWORD") || !(tokenizer.keyWord().equals("var"))) {
            return;
        }
        Symbol.KIND kind = Symbol.KIND.VAR;
        tokenizer.advance();
        String type = getType();
        tokenizer.advance();
        boolean DoneAllVar = false;
        String name;
        do {
            name = tokenizer.identifier();
            symbolTable.define(name, type, kind);
            tokenizer.advance();
            if (tokenizer.tokenType().equals("SYMBOL") && (tokenizer.symbol() == ';')) { // if its a ;
                DoneAllVar = true;
            }      
            tokenizer.advance();
        } while (DoneAllVar == false);
        compileVarDec();
    }

    public void compileSubroutineBody() throws IOException {
        if (SubroutineType.equals("method")) {
            vmWriter.writePush("argument", 0);
            vmWriter.writePop("pointer",0);
        }
        if (SubroutineType.equals("constructor")) {
            vmWriter.writePush("constant",symbolTable.varCount(Symbol.KIND.FIELD));
            vmWriter.writeCall("Memory.alloc", 1);
            vmWriter.writePop("pointer",0);
        }
        compileStatements();
    }

    public void compileStatements() throws IOException {
        if (tokenizer.keyWord().equals("let")) {
            compileLet();
        }
        if (tokenizer.keyWord().equals("return")) {
            compileReturn();
        }
        if (tokenizer.keyWord().equals("do")) {
            compileDo();
        }
        if (tokenizer.keyWord().equals("if")) {
            compileIf();
        }
        if (tokenizer.keyWord().equals("while")) {
            compileWhile();
        }
        if (!tokenizer.tokenType().equals("SYMBOL")) { // if its not a }
            compileStatements();
        }
        
    }

    public void compileLet() throws IOException {
        tokenizer.advance();
        Symbol symbol = symbolTable.find(tokenizer.identifier());
        tokenizer.advance();
        if (tokenizer.symbol() == '[') {
            tokenizer.advance();
            vmWriter.writePush(getSegment(symbol), symbol.getIndex());
            compileExpression();
            vmWriter.writeArithmetic("add");
            tokenizer.advance();
            tokenizer.advance();
            compileExpression();
            tokenizer.advance();
            vmWriter.writePop("temp", 0);
            vmWriter.writePop("pointer", 1);
            vmWriter.writePush("temp", 0);
            vmWriter.writePop("that", 0);

        } else {
            tokenizer.advance();
            compileExpression();
            tokenizer.advance();
            vmWriter.writePop(getSegment(symbol), symbol.getIndex());
        }
    }

    public String getSegment(Symbol symbol) {
        switch (symbol.getKind()) {
            case ARG:
                return "argument";
            case VAR:
                return "local";
            case STATIC:
                return "static";
            default: // FIELD case (we need a default case, arbitrarily chose FIELD)
                return "this";
        }
    }

    public void compileReturn() throws IOException {
        tokenizer.advance();
        if (tokenizer.tokenType().equals("SYMBOL")) { // i.e the token is a semicolon
            vmWriter.writePush("constant", 0);
        } else {
            compileExpression();
        }
        tokenizer.advance();
        vmWriter.writeReturn();
    }

    public void compileDo() throws IOException {
        tokenizer.advance();
        compileExpression();
        tokenizer.advance();
        vmWriter.writePop("temp", 0);
    }

    public void compileIf() throws IOException {
        tokenizer.advance(); 
        tokenizer.advance();
        compileExpression();
        vmWriter.writeArithmetic("not");
        String label1 = newLabel();
        String label2 = newLabel();
        vmWriter.writeIf(label1);
        tokenizer.advance();
        tokenizer.advance();
        compileStatements();
        tokenizer.advance();
        if (tokenizer.keyWord().equals("else")) {
            vmWriter.writeGoto(label2);
            vmWriter.writeLabel(label1);
            tokenizer.advance();
            tokenizer.advance();
            compileStatements();
            vmWriter.writeLabel(label2);
            tokenizer.advance();
        } else {
            vmWriter.writeLabel(label1);
        }        
    }

    public void compileWhile() throws IOException {
        tokenizer.advance(); // go past while
        tokenizer.advance(); // go past  (
        String label1 = newLabel();
        String label2 = newLabel();
        vmWriter.writeLabel(label1);
        compileExpression();
        vmWriter.writeArithmetic("not");
        vmWriter.writeIf(label2);
        tokenizer.advance(); // go past )
        tokenizer.advance(); // go past {
        compileStatements();
        vmWriter.writeGoto(label1);
        vmWriter.writeLabel(label2);
        tokenizer.advance();
    }

    public void compileExpression() throws IOException {
        compileTerm();
        while (tokenizer.tokenType().equals("SYMBOL") && isOp()) {
                Character operator = tokenizer.symbol();
                tokenizer.advance();
                compileTerm();

                switch (operator){
                    case '+':vmWriter.writeArithmetic("add");break;
                    case '-':vmWriter.writeArithmetic("sub");break;
                    case '*':vmWriter.writeCall("Math.multiply", 2);break;
                    case '/':vmWriter.writeCall("Math.divide", 2);break;
                    case '<':vmWriter.writeArithmetic("lt");break;
                    case '>':vmWriter.writeArithmetic("gt");break;
                    case '=':vmWriter.writeArithmetic("eq");break;
                    case '&':vmWriter.writeArithmetic("and");break;
                    case '|':vmWriter.writeArithmetic("or");break;
                }
        }
    }

    public void compileTerm() throws IOException {
        if (tokenizer.tokenType().equals("IDENTIFIER")){
            //varName|varName '[' expression ']'|subroutineCall
            String identifier = tokenizer.identifier();

            tokenizer.advance();
            if (tokenizer.tokenType().equals("SYMBOL") && tokenizer.symbol() == '['){
                //an array entry

                //push array variable,base address into stack
                vmWriter.writePush(getSegment(symbolTable.find(identifier)),symbolTable.indexOf(identifier));
                tokenizer.advance();

                compileExpression();
                tokenizer.advance(); // ]

                //baseAddress + index
                vmWriter.writeArithmetic("add");

                //pop into 'that' pointer
                vmWriter.writePop("pointer",1);
                //push *(base+index) onto stack
                vmWriter.writePush("that",0);

            }else if (tokenizer.tokenType().equals("SYMBOL") && (tokenizer.symbol() == '(' || tokenizer.symbol() == '.')){
                compileSubroutineCall(identifier);
            }else {
                //just a varialbe name
                vmWriter.writePush(getSegment(symbolTable.find(identifier)), symbolTable.indexOf(identifier));
            }

        }else{
            //integerConstant|stringConstant|keywordConstant|'(' expression ')'|unaryOp term
            if (tokenizer.tokenType().equals("INT_CONST")){
                vmWriter.writePush("constant",tokenizer.intVal());
                tokenizer.advance();

            }else if (tokenizer.tokenType().equals("STRING_CONST")){
                //create a new string and append every character to it
                String str = tokenizer.stringVal();

                vmWriter.writePush("constant",str.length());
                vmWriter.writeCall("String.new",1);

                for (int i = 0; i < str.length(); i++){
                    vmWriter.writePush("constant",(int)str.charAt(i));
                    vmWriter.writeCall("String.appendChar",2);
                }
                tokenizer.advance();

            }else if(tokenizer.tokenType().equals("KEYWORD") && tokenizer.keyWord().equals("true")){
                //-1 is true
                vmWriter.writePush("constant",1);
                vmWriter.writeArithmetic("neg");
                tokenizer.advance();

            }else if(tokenizer.tokenType().equals("KEYWORD") && tokenizer.keyWord().equals("this")){
                //push this pointer onto stack
                vmWriter.writePush("pointer",0);
                tokenizer.advance();

            }else if(tokenizer.tokenType().equals("KEYWORD") && (tokenizer.keyWord().equals("false") || tokenizer.keyWord().equals("NULL"))){
                //0 for false and null
                vmWriter.writePush("constant",0);
                tokenizer.advance();

            }else if (tokenizer.tokenType().equals("SYMBOL") && tokenizer.symbol() == '('){
                //expression
                tokenizer.advance();
                compileExpression();
                tokenizer.advance();
                
            }else if (tokenizer.tokenType().equals("SYMBOL") && (tokenizer.symbol() == '-' || tokenizer.symbol() == '~')){

                char s = tokenizer.symbol();
                tokenizer.advance();

                compileTerm();

                if (s == '-'){
                    vmWriter.writeArithmetic("neg");
                }else {
                    vmWriter.writeArithmetic("not");
                }
            }
        }

    }

    public void compileSubroutineCall(String subroutine) throws IOException{
        int nArgs = 0;
        if (tokenizer.tokenType().equals("SYMBOL") && tokenizer.symbol() == '('){
            vmWriter.writePush("pointer",0);
            //'(' expressionList ')'
            tokenizer.advance();
            nArgs = compileExpressionList() + 1;
            tokenizer.advance();
            //call subroutine
            vmWriter.writeCall(currentClass + '.' + subroutine, nArgs);

        }else if (tokenizer.tokenType().equals("SYMBOL") && tokenizer.symbol() == '.'){
            //(className|varName) '.' subroutineName '(' expressionList ')'

            String objName = subroutine;
            tokenizer.advance();
            subroutine = tokenizer.identifier();

            String type = symbolTable.typeOf(objName);

            if (type == null){ // if its a className
                subroutine = objName + "." + subroutine;
            }else { // if its a varName
                nArgs = 1;
                //push variable directly onto stack
                vmWriter.writePush(getSegment(symbolTable.find(objName)), symbolTable.indexOf(objName));
                subroutine = symbolTable.typeOf(objName) + "." + subroutine;
            }
            tokenizer.advance();
            tokenizer.advance(); //'('
            nArgs += compileExpressionList();
            tokenizer.advance(); //')'
            vmWriter.writeCall(subroutine,nArgs);
        }
    }

    public int compileExpressionList() throws IOException {
        if (tokenizer.tokenType().equals("SYMBOL") && (tokenizer.symbol() == ')')) {
            return 0;
        }
        compileExpression();
        int num = 1;
        while (tokenizer.symbol() == ',') {
            tokenizer.advance();
            compileExpression();
            num++;
        }
        return num;
    }

    public boolean isOp(){
        return Arrays.asList(operators).contains(tokenizer.symbol());
    }

    public String newLabel(){
        return "LABEL_" + (labelIndex++);
    }


}
