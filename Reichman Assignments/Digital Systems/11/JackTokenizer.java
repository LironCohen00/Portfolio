import java.util.Scanner;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class JackTokenizer {
    Scanner fileScanner;
    List<String> tokens;
    int TokenCounter;
    String CurrentToken;
    static String[] keywords = {"class" ,"constructor" , "function" , "method" , "field" , "static" ,"var" ,"int", "char" , "boolean",  "void" , "true" , "false" ,"null" , "this" ,"let" ,"do","if" ,"else" , "while" , "return"};
    static String[] Symbols = {"{", "}","(", ")", "[", "]", ".", ",", ";", "+", "-", "*", "/", "&", "|", "<", ">", "=", "~"};

    public JackTokenizer(File file) throws FileNotFoundException {
        fileScanner = new Scanner(file);
        Scanner fileScanner = new Scanner(file);
        tokens = new ArrayList<String>();
        String currentline;
        boolean skip = false;
        /// adding all the tokens into the token list ----------------------------------------------------------------------------
        while (fileScanner.hasNextLine()) {
            ////////////////// looping through the lines until a valid line of code that can be tokenized is found ////////////////////////////
            do {
                currentline = fileScanner.nextLine().trim();
                if (currentline.indexOf("//") >= 0 ) {
                    currentline = currentline.substring(0, currentline.indexOf("//")).trim(); // removing the comment after the line of code
                }
                if ((currentline.indexOf("/*") == 0) && (currentline.indexOf("*/") < 0)) {
                    skip = true;
                } else if ((currentline.indexOf("/*") > 0) && (currentline.indexOf("*/") >= 0)) {
                    currentline = currentline.substring(0, currentline.indexOf("/*")).trim(); //if there is valid code before the /*
                }else if (currentline.indexOf("*/") >= 0) {
                    skip = false;
                    currentline = currentline.substring(currentline.indexOf("*/") + 2).trim(); 
                    //  if theres code after the "*/", then that code is valid and must be tokenized, else currentline will be empty
                    //  and the while loop will ensure that we move onto the next line
                }
            }while (((currentline == "") || (skip == true)) && (fileScanner.hasNextLine()));
            ///////////////////////////////////////////////////////////////////////////////////////////////////
            if ((currentline == "") && (!fileScanner.hasNextLine())){ // if the last line is a comment, a special edge case
                break;                                               
            }
            /// tokenize the valid line of code and add them into the tokens list
            String[] temp = (currentline.split("((?=[\\s\"(),\\[\\]+\\-*/&|~.<>={};])|(?<=[\\s(),\"<>\\[\\]+\\-*/&|~.={};]))")); 
            int numQuotations = 0;
            String token = "";
            for (int i = 0; i < temp.length; i++) {
                if ((numQuotations%2 == 1) || (temp[i].equals("\""))) {
                    token = token + temp[i];
                } else {
                    if ((temp[i].length() != 0) && (temp[i].charAt(0) != ' ')) { // this only happens if the whole string is just a space (due to the nature of the split)
                        tokens.add(temp[i]);                                            // note, have to check (temp[i].length() != 0) to ensure there is no indexOutOfBoundsError
                    }
                    token = "";
                }
                if (temp[i].equals("\"")) {
                    numQuotations++;
                    if (numQuotations%2 == 0) {
                        tokens.add(token);
                    }
                }
                
            }
        }
        //-----------------------------------------------------------------------------------------------------------------------
        fileScanner.close();
        TokenCounter = 0;
        CurrentToken = tokens.get(0);
    }

    public boolean hasMoreTokens() {
        return (TokenCounter + 1 < tokens.size());
    }

    public void advance() {
        if (hasMoreTokens()) {
            TokenCounter++;
            CurrentToken = tokens.get(TokenCounter);
        }
    }

    public final String tokenType() {
        if (Arrays.asList(keywords).contains(CurrentToken)) {
            return "KEYWORD";
        }
        if (Arrays.asList(Symbols).contains(CurrentToken)) {
            return "SYMBOL";
        }
        try {
            Integer.parseInt(CurrentToken);
            return "INT_CONST";
          } catch (NumberFormatException e) {

          }
        if (CurrentToken.indexOf("\"") >= 0) {
            return "STRING_CONST";
        }
        return "IDENTIFIER";
    }

    public final String keyWord() {
        return CurrentToken;
    }

    public final char symbol() {
        return CurrentToken.charAt(0);
    }

    public final String identifier() {
        return CurrentToken;
    }

    public final int intVal() {
        return Integer.parseInt(CurrentToken);
    }

    public final String stringVal() {
        //System.out.println(CurrentToken);
        return CurrentToken.substring(1, CurrentToken.length() - 1);
    }

    public void printTokens() {
        TokenCounter = 0;
        while (hasMoreTokens()) {
            System.out.println(CurrentToken);
            advance();
        }
        TokenCounter = 0;
    }
}