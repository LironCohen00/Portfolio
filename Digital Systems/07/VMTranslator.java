import java.io.File;
import java.io.IOException;

public class VMTranslator {
    public static void main(String[] args) throws IOException {
        File file = new File(args[0]);
        String fileName = args[0]; // fileName starts off as the whole path

        // reduce fileName to the actual file name ///////////////////////////////
        while (fileName.indexOf("\\") >= 0) {
            fileName = fileName.substring(fileName.indexOf("\\") + 1);
        }
        String fileDir = args[0].substring(0 , args[0].length() - fileName.length());  //file path minus the file name at the end  
        fileName = fileName.substring(0, fileName.indexOf(".")); //removing the extension
        File outputFile = new File(fileDir + fileName + ".asm");
        
        Parser parser = new Parser(file);
        CodeWriter codeWriter = new CodeWriter(outputFile, fileName);

        while (parser.hasMoreLines()) {
            parser.advance();
            String command = parser.commandType();
            if (command == "C_ARITHMETIC") {
                codeWriter.writeArithmetic(parser.arg1());
            } else {
                codeWriter.writePushPop(command, parser.arg1(), parser.arg2());
            }
        }
        codeWriter.close();
    }
}
