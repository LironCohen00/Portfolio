import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Parser {
    private Scanner scanner;
    private String currentInstruction;
    private String[] words;
    
    public Parser(File file) throws FileNotFoundException {
        scanner = new Scanner(file);
    } 

    public boolean hasMoreLines() {
        return scanner.hasNextLine();
    }

    public void advance() {
        if (this.hasMoreLines()) {
            do { 
                currentInstruction = scanner.nextLine().trim();
            } while ( (currentInstruction.isEmpty()) || (currentInstruction.charAt(0) == '/') ); //if the line is just a comment or an empty line
            if (currentInstruction.indexOf("/") > 0) {
                currentInstruction = currentInstruction.substring(0, currentInstruction.indexOf("/"));
            }
            words = currentInstruction.split("\s+");
        }
    }

    public String commandType() {
        switch (words[0]) {
            case "push":
                return "C_PUSH";
            case "pop":
                return "C_POP";
            case "add":
            case "sub":
            case "neg":
            case "eq":
            case "gt":
            case "lt":
            case "and":
            case "or":
            case "not":
                return "C_ARITHMETIC";
            case "label":
                return "C_LABEL";
            case "function":
                return "C_FUNCTION";
            case "if-goto":
                return "C_IF";
            case "goto":
                return "C_GOTO";
            case "return":
                return "C_RETURN";
            default: // "call" case
                return "C_CALL";
        }
    }

    public String arg1() {
        if (this.commandType() == "C_ARITHMETIC") {
            return words[0];
        }
        if (this.commandType() == "C_RETURN") {
            return null;
        }
        return words[1]; // if its not arithemtic or a return
    }

    public int arg2() {
        words[2] = words[2].trim();
        return Integer.parseInt(words[2]);
    }

}
