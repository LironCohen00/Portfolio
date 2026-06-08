import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class VMWriter {
    FileWriter filewriter;

    public VMWriter(File file) throws IOException {
        filewriter = new FileWriter(file);
    }

    public void writePush(String segment, int index) throws IOException {
        filewriter.write("push "  + segment + " " + index  + "\n");
    }
    public void writePop(String segment, int index) throws IOException {
        filewriter.write("pop "  + segment + " " + index + "\n");
    }

    public void writeArithmetic(String command) throws IOException {
        filewriter.write(command  + "\n");
    }

    public void writeLabel(String label) throws IOException {
        filewriter.write("(" + label + ")\n");
    }

    public void writeGoto(String label) throws IOException {
        filewriter.write("goto "  + label + "\n");
    }

    public void writeIf(String label) throws IOException {
        filewriter.write("if-goto "  + label + "\n");
    }

    public void writeCall(String name, int nArgs) throws IOException {
        filewriter.write("call "  + name + " " + nArgs + "\n");
    }

    public void writeFunction(String name, int nVars) throws IOException {
        filewriter.write("function "  + name + " " + nVars + "\n");
    }

    public void writeReturn() throws IOException {
        filewriter.write("return\n");
    }

    public void close() throws IOException {
        filewriter.close();
    }
}
