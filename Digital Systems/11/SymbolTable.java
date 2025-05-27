import java.util.HashMap;
import java.util.Map;

public class SymbolTable {
    private HashMap<String,Symbol> classSymbols; 
    private HashMap<String,Symbol> subroutineSymbols;
    private HashMap<Symbol.KIND,Integer> NumKind;

    public SymbolTable() {
        subroutineSymbols = new HashMap<String, Symbol>();
        classSymbols = new HashMap<String, Symbol>();
        NumKind = new HashMap<Symbol.KIND, Integer>();
        NumKind.put(Symbol.KIND.FIELD,0);
        NumKind.put(Symbol.KIND.ARG,0);
        NumKind.put(Symbol.KIND.VAR,0);
        NumKind.put(Symbol.KIND.STATIC,0);
    }

    public void startSubroutine(){
        subroutineSymbols.clear();
        NumKind.put(Symbol.KIND.ARG,0);
        NumKind.put(Symbol.KIND.VAR,0);
    }

    public void define(String name, String type, Symbol.KIND kind){
        int index = NumKind.get(kind);
        Symbol symbol = new Symbol(type,kind,index);
        NumKind.put(kind,index + 1);
        if (kind == Symbol.KIND.ARG || kind == Symbol.KIND.VAR){
            subroutineSymbols.put(name,symbol);
        }else if(kind == Symbol.KIND.STATIC || kind == Symbol.KIND.FIELD){
            classSymbols.put(name,symbol);
        }

    }

    public int varCount(Symbol.KIND kind){
        return NumKind.get(kind);
    }

    public Symbol.KIND kindOf(String name){
        Symbol symbol = find(name);
        if (symbol == null) {
            return Symbol.KIND.NONE;
        } else {
            return symbol.getKind();
        }
    }

    public Symbol find(String name) {
        Symbol ClassSymbol = classSymbols.get(name);
        Symbol SubroutineSymbol = subroutineSymbols.get(name);
        if ((ClassSymbol == null) && (SubroutineSymbol == null)) {
            return null;
        } else if (ClassSymbol != null) {
            return ClassSymbol;
        } else {
            return SubroutineSymbol;
        }
    }

    public String typeOf(String name){
        Symbol symbol = find(name);
        if (symbol == null) return null;
        return symbol.getType();
    }

    public int indexOf(String name){
        Symbol symbol = find(name);
        if (symbol == null) return -1;
        return symbol.getIndex();
    }

}
