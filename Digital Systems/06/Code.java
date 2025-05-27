public class Code {
    private String a;


    public Code() {

    }

    public String dest(String dest) {
        String BinaryDest = "000";
        if (dest == null) {return BinaryDest;}
        if (dest.indexOf("A") != -1) {
            BinaryDest = "1" + BinaryDest.substring(1);
        }
        if (dest.indexOf("D") != -1) {
            BinaryDest = BinaryDest.charAt(0) + "1" + BinaryDest.charAt(2);
        }
        if (dest.indexOf("M") != -1) {
            BinaryDest = BinaryDest.substring(0, 2) + "1";
        }
        return BinaryDest;
    }
    public String comp(String comp) {
        a = "0";
        switch (comp) {
            case "1":
                return "111111";
            case "-1":
                return "111010";
            case "D":
                return "001100";
            case "A":
                return "110000";
            case "M":
                a = "1";
                return "110000";
            case "!D":
                return "001101";
            case "!A":
                 return "110001";
            case "!M":
                a = "1";
                return "110001";
            case "-D":
                return "001111";
            case "-A":
                return "110011";
            case "-M":
                a = "1";
                return "110011";
            case "D+1":
                return "011111";
            case "A+1":
                return "110111";
            case "M+1":
                a = "1";
                return "110111";
            case "D-1":
                return "001110";
            case "A-1":
                return "110010";
            case "M-1":
                a = "1";
                return "110010";
            case "D+A":
                return "000010";
            case "D+M":
                a = "1";
                return "000010";
            case "D-A":
                return "010011";
            case "D-M":
                a = "1";
                return "010011";
            case "A-D":
                return "000111";
            case "M-D":
                a = "1";
                return "000111";
            case "D&A":
                return "000000";
            case "D&M":
                a = "1";
                return "000000";
            case "D|A":
                return "010101";  
            case "D|M":
                a = "1";
                return "010101";    
            default: // our default will be "0" since we are assuming there must be a case match
                return "101010";
        }
    }
    public String jump(String jump) {
        if (jump == null) {return "000";}
        switch (jump) {
            case "JGT":
                return "001";
            case "JEQ":
                return "010";
            case "JGE":
                return "011";
            case "JLT":
                return "100";
            case "JNE":
                return "101";
            case "JLE":
                return "110";
            default: // "JMP" will be our default
                return "111";
        }
    }

    public String get_a() {
        return a;
    }
}
