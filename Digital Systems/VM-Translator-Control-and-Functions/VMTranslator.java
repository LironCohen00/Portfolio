import java.io.File;
import java.io.IOException;

public class VMTranslator {
    public static void main(String[] args) throws IOException {
        File file = new File(args[0]);
        File[] inputFiles;
        File outputFile;
        CodeWriter codeWriter;
        String FileName;

        inputFiles = file.listFiles((sFile) -> sFile.getName().endsWith(".vm"));
        if (inputFiles == null) { // if the input is a file (if the input is a file, then listFiles returns null)
            inputFiles = new File[1];
            inputFiles[0] = file;
            FileName = file.getName().substring(0, file.getName().indexOf(".")); //removing the extension              
            outputFile = new File(file.getParent() + "\\" +  FileName + ".asm");
            codeWriter = new CodeWriter(outputFile, inputFiles[0].getName(), false);
        } else { // if the input is a directory
            outputFile = new File(file.getAbsolutePath() + "\\" + file.getName() + ".asm");
            codeWriter = new CodeWriter(outputFile, inputFiles[0].getName(), true);
        }   
        for (int i = 0; i < inputFiles.length; i++) {
            FileName = inputFiles[i].getName().substring(0, inputFiles[i].getName().indexOf(".")); //removing the extension  
            codeWriter.setFileName(FileName);
            Parser parser = new Parser(inputFiles[i]);
            while (parser.hasMoreLines()) {
                parser.advance();
                String command = parser.commandType();
                if (command == "C_ARITHMETIC") {
                    codeWriter.writeArithmetic(parser.arg1());
                } else if ((command == "C_PUSH") || (command == "C_POP")) {
                    codeWriter.writePushPop(command, parser.arg1(), parser.arg2());
                } else if (command == "C_LABEL") {
                    codeWriter.writeLabel(parser.arg1());
                } else if (command == "C_FUNCTION") {
                    codeWriter.writeFunction(parser.arg1(),parser.arg2());
                } else if (command == "C_IF") {
                    codeWriter.writeIf(parser.arg1());
                } else if (command == "C_GOTO") {
                    codeWriter.writeGoto(parser.arg1());
                } else if (command == "C_RETURN") {
                    codeWriter.writeReturn();
                } else { // "C_RETURN"
                    codeWriter.writeCall(parser.arg1(), parser.arg2());
                }
            }
        }
        codeWriter.close();
    }
}
