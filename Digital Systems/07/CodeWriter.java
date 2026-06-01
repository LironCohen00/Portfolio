import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class CodeWriter {
    private FileWriter fileWriter;
    private String fileName;
    private int LineCount;

    public CodeWriter(File file, String fileName) throws IOException {
        fileWriter = new FileWriter(file);
        LineCount = -1; //initialise to -1 since the first line of code is on line 0
        this.fileName = fileName;
    }

    public void writeArithmetic(String command) throws IOException {
        String Code;
        int JumpAddress1;
        int JumpAddress2;
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
}
