import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class CodeWriter {
    private FileWriter fileWriter;
    private String fileName;
    private int LineCount;
    private int NumLabel;

    public CodeWriter(File file, String fileName, boolean Directory) throws IOException {
        fileWriter = new FileWriter(file);
        LineCount = -1; //initialise to -1 since the first line of code is on line 0
        setFileName(fileName);
        NumLabel = 0;
        if (Directory) {
            fileWriter.write("@256\n" + "D=A\n" + "@SP\n" + "M=D\n");
            LineCount += 4;
            writeCall("Sys.init", 0);
        }
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }
 

    public void writeArithmetic(String command) throws IOException {
        String Code;
        int JumpAddress1;
        int JumpAddress2;
        //fileWriter.write(command + "\n");
        // we have seperated each assembly line of code with a "+"" for the sake of clarity and readability, we know we can make the program more efficient by removing each "+"
        switch (command) {
            case "add":
                Code = "@SP\n" + "A=M-1\n" + "A=A-1\n"  + "D=M\n" + "A=A+1\n" + "D=D+M\n" + "A=A-1\n" + "M=D\n" + "@SP\n" + "M=M-1\n";
                LineCount += 10;
                break;
            case "sub":
                Code = "@SP\n" + "A=M-1\n" + "A=A-1\n" + "D=M\n" + "A=A+1\n" + "D=D-M\n" + "A=A-1\n" + "M=D\n" + "@SP\n" + "M=M-1\n";
                LineCount += 10;
                break;
            case "neg":
                Code = "@SP\n" + "A=M-1\n" + "D=M\n" + "M=M-D\n" + "M=M-D\n";
                LineCount += 5;
                break;
            case "eq":
                JumpAddress1 = LineCount + 14; 
                JumpAddress2 = LineCount + 18;
                Code = "@SP\n" + "A=M-1\n"  + "D=M\n" + "A=A-1\n" + "D=M-D\n"  + "@" + JumpAddress1 + "\n" + "" + "D;JEQ\n" + "@SP\n" + "A=M-1\n" + 
                    "A=A-1\n" + "M=0\n" + "@" + JumpAddress2 + "\n" + "0;JMP\n"  + "@SP\n" + "A=M-1\n" + "A=A-1\n" + "M=-1\n"  + "@SP\n" + "M=M-1\n";
                LineCount += 19;
                break;
            case "gt":
                JumpAddress1 = LineCount + 14; 
                JumpAddress2 = LineCount + 18;
                Code = "@SP\n" + "A=M-1\n"  + "D=M\n" + "A=A-1\n" + "D=M-D\n"  + "@" + JumpAddress1 + "\n" + "" + "D;JGT\n" + "@SP\n" + "A=M-1\n" + 
                    "A=A-1\n" + "M=0\n" + "@" + JumpAddress2 + "\n" + "0;JMP\n"  + "@SP\n" + "A=M-1\n" + "A=A-1\n" + "M=-1\n"  + "@SP\n" + "M=M-1\n";
                LineCount += 19;
                break;
            case "lt":
                JumpAddress1 = LineCount + 14; 
                JumpAddress2 = LineCount + 18;
                Code = "@SP\n" + "A=M-1\n"  + "D=M\n" + "A=A-1\n" + "D=M-D\n"  + "@" + JumpAddress1 + "\n" + "" + "D;JLT\n" + "@SP\n" + "A=M-1\n" + 
                    "A=A-1\n" + "M=0\n" + "@" + JumpAddress2 + "\n" + "0;JMP\n"  + "@SP\n" + "A=M-1\n" + "A=A-1\n" + "M=-1\n"  + "@SP\n" + "M=M-1\n";
                LineCount += 19;
                break;
            case "and":
                Code = "@SP\n" + "A=M-1\n" + "A=A-1\n"  + "D=M\n" + "A=A+1\n" + "D=D&M\n" + "A=A-1\n" + "M=D\n" + "@SP\n" + "M=M-1\n";
                LineCount += 10;
                break;
            case "or":
                Code = "@SP\n" + "A=M-1\n" + "A=A-1\n"  + "D=M\n" + "A=A+1\n" + "D=D|M\n" + "A=A-1\n" + "M=D\n" + "@SP\n" + "M=M-1\n";
                LineCount += 10;
                break;
            default: // "not" case
                Code = "@SP\n" + "A=M-1\n" + "M=!M\n";
                LineCount += 3;
        }
        fileWriter.write(Code);
    }

    public void writePushPop(String command, String segment, int index) throws IOException {
        String Code;
        //fileWriter.write(command + segment + index + "\n");
         // we have seperated each assembly line of code with a "+"" for the sake of clarity and readability, we know we can make the program more efficient by removing alot of the "+"
        if (command == "C_PUSH") {
            switch (segment) {
                case "constant":
                    Code = "@" + index + "\n" + "D=A\n" + "@SP\n" + "A=M\n" + "M=D\n"  + "@SP\n" + "M=M+1\n";
                    LineCount += 7;
                    break;
                case "local":
                    Code = "@" + index + "\n" + "D=A\n"  + "@LCL\n" + "A=M+D\n" + "D=M\n"  + "@SP\n"  + "A=M\n"  + "M=D\n"  + "@SP\n"  + "M=M+1\n";
                    LineCount += 10;
                    break;
                case "static":
                    Code = "@" + fileName + "." + index + "\n" + "D=M\n"  + "@SP\n" + "A=M\n" + "M=D\n" + "@SP\n" + "M=M+1\n";
                    LineCount += 7;
                    break;
                case "this":
                    Code = "@" + index + "\n" + "D=A\n"  + "@THIS\n" + "A=M+D\n" + "D=M\n"  + "@SP\n"  + "A=M\n"  + "M=D\n"  + "@SP\n"  + "M=M+1\n";
                    LineCount += 10;
                    break;
                case "that":
                    Code = "@" + index + "\n" + "D=A\n"  + "@THAT\n" + "A=M+D\n" + "D=M\n"  + "@SP\n"  + "A=M\n"  + "M=D\n"  + "@SP\n"  + "M=M+1\n";
                    LineCount += 10;
                    break;
                case "argument":
                    Code = "@" + index + "\n" + "D=A\n"  + "@ARG\n" + "A=M+D\n" + "D=M\n"  + "@SP\n"  + "A=M\n"  + "M=D\n"  + "@SP\n"  + "M=M+1\n";
                    LineCount += 10; 
                    break;
                case "pointer":
                    if (index == 0) {
                        Code = "@THIS\n" + "D=M\n"  + "@SP\n"  + "A=M\n"  + "M=D\n"  + "@SP\n"  + "M=M+1\n";
                    }else {
                        Code = "@THAT\n" + "D=M\n"  + "@SP\n"  + "A=M\n"  + "M=D\n"  + "@SP\n"  + "M=M+1\n";
                    }
                    LineCount += 7;
                    break;
                default: // if its "temp"
                    Code = "@" + index + "\n" + "D=A\n"  + "@5\n" + "A=A+D\n" + "D=M\n"  + "@SP\n"  + "A=M\n"  + "M=D\n"  + "@SP\n"  + "M=M+1\n";
                    LineCount += 10;
                }   
        } else { // "C_POP" command
            switch (segment) {
                case "local":
                    Code = "@" + index + "\n" + "D=A\n" + "@LCL\n" + "D=M+D\n" + "@R13\n" + "M=D\n" + "@SP\n" + "AM=M-1\n" + "D=M\n" + "@R13\n" + "A=M\n" + "M=D\n";
                    LineCount += 12;
                    break;
                case "static":
                    Code = "@SP\n"  + "AM=M-1\n" + "D=M\n" + "@" + fileName + "." + index + "\n" + "M=D\n";
                    LineCount += 5;
                    break;
                case "this":
                    Code = "@" + index + "\n" + "D=A\n" + "@THIS\n" + "D=M+D\n" + "@R13\n" + "M=D\n" + "@SP\n" + "AM=M-1\n" + "D=M\n" + "@R13\n" + "A=M\n" + "M=D\n";
                    LineCount += 12;
                    break;
                case "that":
                    Code = "@" + index + "\n" + "D=A\n" + "@THAT\n" + "D=M+D\n" + "@R13\n" + "M=D\n" + "@SP\n" + "AM=M-1\n" + "D=M\n" + "@R13\n" + "A=M\n" + "M=D\n";
                    LineCount += 12;
                    break;
                case "argument":
                    Code = "@" + index + "\n" + "D=A\n" + "@ARG\n" + "D=M+D\n" + "@R13\n" + "M=D\n" + "@SP\n" + "AM=M-1\n" + "D=M\n" + "@R13\n" + "A=M\n" + "M=D\n";
                    LineCount += 12;
                    break;
                case "pointer":
                    if (index == 0) {
                        Code = "@SP\n" + "A=M-1\n" + "D=M\n" + "@THIS\n"  + "M=D\n"  + "@SP\n" + "M=M-1\n";  
                    }else {
                        Code = "@SP\n" + "A=M-1\n" + "D=M\n" + "@THAT\n"  + "M=D\n"  + "@SP\n" + "M=M-1\n";
                    }
                    LineCount += 7;
                    break;
                default: // if its "temp"
                    Code = "@" + index + "\n" + "D=A\n" + "@5\n" + "D=A+D\n" + "@R13\n" + "M=D\n" + "@SP\n" + "AM=M-1\n" + "D=M\n" + "@R13\n" + "A=M\n" + "M=D\n";   
                    LineCount += 12;
                }
        }
        fileWriter.write(Code);
    }

    public void close() throws IOException {
        fileWriter.close();
    }

    public void writeLabel(String label) throws IOException{
        fileWriter.write("(" + label + ")\n");
        LineCount++;
    }

    public void writeGoto(String label) throws IOException {
        //fileWriter.write("goto " + label + "\n");
        fileWriter.write("@" + label + "\n0;JMP\n");
        LineCount += 2;
    }

    public void writeIf(String label) throws IOException {
        //fileWriter.write("if-goto " + label + "\n");
        writePushPop("C_POP", "temp", 0);
        fileWriter.write("@5\n" + "D=M\n" + "@" + label + "\nD;JNE\n");
        LineCount += 5;
    }

    public void writeFunction(String functionName, int nVars) throws IOException {
        fileWriter.write("(" + functionName + ")\n");
        LineCount++;
        for (int i = 0; i < nVars; i++) {
            writePushPop("C_PUSH", "constant", 0);
        }
    }

    public void writeCall(String functionName, int nArgs) throws IOException {
        //fileWriter.write("call " + functionName + nArgs + "\n");
        // Saving the frame of the caller --------------------------------------------------------------------
        int retAddress = LineCount + 46; // the 46 is is how many lines of code will be written before/until the "(retAddress)" line of code
        writePushPop("C_PUSH", "constant", retAddress); //lineCount will be incremented in the method
        fileWriter.write("@LCL\n" + "D=M\n" + "@SP\n" + "A=M\n" + "M=D\n" + "@SP\n" + "M=M+1\n");
        fileWriter.write("@ARG\n" + "D=M\n" + "@SP\n" + "A=M\n" + "M=D\n" + "@SP\n" + "M=M+1\n");
        fileWriter.write("@THIS\n" + "D=M\n" + "@SP\n" + "A=M\n" + "M=D\n" + "@SP\n" + "M=M+1\n");
        fileWriter.write("@THAT\n" + "D=M\n" + "@SP\n" + "A=M\n" + "M=D\n" + "@SP\n" + "M=M+1\n");
        LineCount += 28; 

        // Reposition ARG and LCL pointer --------------------------------------------------------------------------
        int temp = 5 + nArgs;
        fileWriter.write("@" + temp + "\n" + "D=A\n" + "@SP\n" + "D=M-D\n" + "@ARG\n" + "M=D\n");
        fileWriter.write("@SP\n" + "D=M\n" + "@LCL\n" + "M=D\n");
        LineCount += 10;
        
        // Goto function ------------------------------------------------------------------------------------
        writeGoto(functionName);
        fileWriter.write("(" + "RETURN" + NumLabel + ")\n");
        NumLabel++;
        LineCount++;
    }

    public void writeReturn() throws IOException {
        //fileWriter.write("return\n");
        fileWriter.write("@LCL\n" + "D=M\n" + "@5\n" + "M=D\n"); // store the address of LCL in temp segement i.e our endFrame
        fileWriter.write("@5\n" + "D=A\n" + "A=M-D\n" + "D=M\n" + "@6\n" + "M=D\n"); // store the retAddress in the temp segment
        LineCount += 10; 
        writePushPop("C_POP", "argument", 0); // *ARG = pop()
        fileWriter.write("@ARG\n" + "D=M\n" + "@SP\n" + "M=D+1\n"); // SP = ARG + 1
        fileWriter.write("@5\n" + "A=M-1\n" + "D=M\n" + "@THAT\n" + "M=D\n"); // THAT = *(endFrame - 1)
        fileWriter.write("@2\n" + "D=A\n" + "@5\n" + "A=M-D\n" + "D=M\n"+ "@THIS\n" + "M=D\n"); // THIS = *(endFrame - 2)
        fileWriter.write("@3\n" + "D=A\n" + "@5\n" + "A=M-D\n" + "D=M\n"+ "@ARG\n" + "M=D\n"); // ARG = *(endFrame - 3)
        fileWriter.write("@4\n" + "D=A\n" + "@5\n" + "A=M-D\n" + "D=M\n"+ "@LCL\n" + "M=D\n"); // LCL = *(endFrame - 4)
        fileWriter.write("@6\n" + "A=M\n" + "0;JMP\n"); // goto retAddress
        LineCount += 33; 
    }

}
