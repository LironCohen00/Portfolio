import java.io.File;
import java.util.Scanner;
import java.io.FileNotFoundException;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class Parser {
    private Scanner fileReader;
    private String currentInstruction;
    private String InstructionType; // storing instruction type as a field because we use it often in different functions, so 
                                    // we don't want to re-compute the instruction type every time we need to use it

    public Parser(File file) throws FileNotFoundException {
        this.fileReader = new Scanner(file);
    }

    public boolean hasMoreLines() {
        return fileReader.hasNextLine();
    }

    public String advance() {
        do { 
            currentInstruction = fileReader.nextLine().trim();
        } while ( (currentInstruction == "") || (currentInstruction.charAt(0) == '/') ); //if the line is just a comment or an empty line
        
        //removing the comment from the line of code
        if (currentInstruction.indexOf('/') != -1) { //if there is a comment after the line of code
            currentInstruction = currentInstruction.substring(0, currentInstruction.indexOf('/'));
            currentInstruction = currentInstruction.trim();
        }
        return currentInstruction;
    }

    public String instructionType() {
        if (currentInstruction == "") {return null;}
        if (currentInstruction.charAt(0) == '@') {
            InstructionType = "A_INSTRUCTION";
            return "A_INSTRUCTION";
        } else if (currentInstruction.charAt(0) == '(') {
                InstructionType = "L_INSTRUCTION";
                return "L_INSTRUCTION";     
        }  
        InstructionType = "C_INSTRUCTION";
        return "C_INSTRUCTION";
    }

    public String symbol() {
        // using Regular Expression package to find if, given that we are working with an A_INSTRUCTION, there is a character
        // that is not a letter (i.e this means it is not a variable, but just a number after the @)
        Pattern pattern = Pattern.compile("[a-z]", Pattern.CASE_INSENSITIVE);  //defining the pattern we're gonna look for as any non-letter character 
        Matcher matcher = pattern.matcher(currentInstruction.substring(1)); 
        boolean LetterFound = matcher.find(); //whether the pattern was found in the A_INSTRUCTON after the "@"
        
        if (InstructionType == "L_INSTRUCTION") {
            return currentInstruction.substring(1, currentInstruction.length() - 1);
        } else if ( (InstructionType == "A_INSTRUCTION") && (LetterFound)) {
            return currentInstruction.substring(1);
        } else {return null;}
    }

    public String dest() {
        if ( (InstructionType == "C_INSTRUCTION") && (currentInstruction.indexOf("=") != -1) ) {
            return currentInstruction.substring(0,currentInstruction.indexOf("="));
        } 
        return null;
    }

    public String comp() {
        if (InstructionType == "C_INSTRUCTION") {
            if (currentInstruction.indexOf(";") != -1) {
                // return string from after '=' sign to before ';' character
                return currentInstruction.substring(currentInstruction.indexOf("=") + 1, currentInstruction.indexOf(";"));
            } 
            if (currentInstruction.indexOf("/") != -1) { //if there is a comment on the line
                // return string from after '=' sign to before '/' character
                return currentInstruction.substring(currentInstruction.indexOf("=") + 1, currentInstruction.indexOf("/")); 
            }
            // we now know that there is no comment or jump clause, therefore comp is from '=' sign to end of line
            return currentInstruction.substring(currentInstruction.indexOf("=") + 1);
        }
        return null; //if not a c instruction 
    }

    public String jump() {
        if ( (InstructionType == "C_INSTRUCTION") && (currentInstruction.indexOf(";") != -1) )  {
            return currentInstruction.substring(currentInstruction.indexOf(";") + 1);
        } 
        return null;  
    }
}
