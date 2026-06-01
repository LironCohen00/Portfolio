import java.util.HashMap;
import java.util.HashMap;

public class SymbolTable {
    private HashMap<String, Integer> Table;
    
    public SymbolTable() {
        Table = new HashMap<String, Integer>();
    }

    public boolean contains(String Symbol) {
        return Table.containsKey(Symbol);
    } 

    public void addEntry(String Symbol, int address) {
        if (!contains(Symbol)) {
            Table.put(Symbol, address);
        }
    }

    public int getAddress(String Symbol) {
        if (contains(Symbol)) {
            return Table.get(Symbol);
        } else {
            return -1;
        }
    }
}
