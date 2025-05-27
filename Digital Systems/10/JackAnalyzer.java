import java.io.File;
import java.io.IOException;

public class JackAnalyzer {
    public static void main(String[] args) throws IOException {
        File file = new File(args[0]);
        File[] inputFiles;
        File outputFile;
        CompilationEngine compilationEngine;
        String FileName;

        inputFiles = file.listFiles((sFile) -> sFile.getName().endsWith(".jack"));
        if (inputFiles == null) { // if the input is a file (if the input is a file, then listFiles returns null)
            inputFiles = new File[1];
            inputFiles[0] = file;                
        } 
        
        for (int i = 0; i < inputFiles.length; i++) {  
            FileName = inputFiles[i].getName().substring(0, inputFiles[i].getName().indexOf(".")); //removing the extension
            outputFile = new File(inputFiles[i].getParent() + "\\" +  FileName + ".xml");
            compilationEngine = new CompilationEngine(inputFiles[i], outputFile);
            compilationEngine.compileClass();
        }
    }
}
