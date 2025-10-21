import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;

public class HackAssembler {
    public static void main (String[] args) throws FileNotFoundException {    
        File file = new File(args[0]);
        String fileName = args[0]; // fileName starts off as the whole path

        // reduce fileName to the actual file name ///////////////////////////////
        while (fileName.indexOf("\\") >= 0) {
            fileName = fileName.substring(fileName.indexOf("\\") + 1);
        }
        String fileDir = args[0].substring(0 , args[0].length() - fileName.length());  //file path minus the file name at the end

        // adding R0-R15 to symbol table /////////////////////////////////////////
        SymbolTable SymTable = new SymbolTable();
        for (int i = 0; i < 16; i++) {
            SymTable.addEntry("R" + i, i);
        }
        SymTable.addEntry("SP", 0);
        SymTable.addEntry("LCL", 1);
        SymTable.addEntry("ARG", 2);
        SymTable.addEntry("THIS", 3);
        SymTable.addEntry("THAT", 4);
        SymTable.addEntry("SCREEN", 16384);
        SymTable.addEntry("KBD", 24576);
        Parser firstParse = new Parser(file);

        // first parse, adding non predefined labels to the symboltable //////////////////////
        int LineCount = 0;
        while (firstParse.hasMoreLines()) {
            firstParse.advance();
            if (firstParse.instructionType() == "L_INSTRUCTION") {
                SymTable.addEntry(firstParse.symbol(), LineCount);
            } else {LineCount++;} // only count the lines with actual instructions, not labels
        }

        // second parse ////////////////////////////////////////////////////////////////
        fileName = fileName.substring(0, fileName.indexOf(".")); //removing the extension
        try {
            FileWriter MachineCode = new FileWriter(fileDir + fileName + ".hack");
            Parser secondParse = new Parser(file);
            int Counter = 16;
            while (secondParse.hasMoreLines()) {
                String CodeLine = secondParse.advance();
                // A INSTRUCTIONS
                if (secondParse.instructionType() == "A_INSTRUCTION") { //if A_INSTRUCTION 
                    if (secondParse.symbol() != null) { // if its a symbol
                        if (!SymTable.contains(secondParse.symbol())) {
                        SymTable.addEntry(secondParse.symbol(), Counter);
                        Counter++;
                        }
                        int address = SymTable.getAddress(secondParse.symbol());
                        String sAddress = String.format("%16s", Integer.toBinaryString(address)).replace(' ', '0');
                        MachineCode.write(sAddress + "\n");
                    } else { // if its not a symbol, but a constant
                        int address = Integer.parseInt(CodeLine.substring(1));
                        String sAddress = String.format("%16s", Integer.toBinaryString(address)).replace(' ', '0');
                        MachineCode.write(sAddress + "\n");
                    }
                    
                }

                // C INSTRUCTIONS
                if (secondParse.instructionType() == "C_INSTRUCTION") {
                    Code code = new Code();
                    String comp = code.comp(secondParse.comp()); //have to call secondparse.comp() beforehand so we can get the correct value for our "a" bit
                    String BinaryCode = "111" + code.get_a();
                    BinaryCode += comp + code.dest(secondParse.dest()) + code.jump(secondParse.jump());
                    MachineCode.write(BinaryCode + "\n");
                }
            }
            MachineCode.close();

        } catch (IOException e) {
            System.out.println(e);
        }
        
    }
}