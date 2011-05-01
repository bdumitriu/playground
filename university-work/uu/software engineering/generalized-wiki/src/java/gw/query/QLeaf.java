package gw.query;

/**
 * Part of the tree datastructure to store the bibtex queries
 */
public class QLeaf {
	public static int EQUAL    = 0; // ==
    public static int NOTEQUAL = 1; // !=
    public static int LT       = 2; // <
    public static int LTE      = 3; // <=
    public static int GT       = 4; // >
    public static int GTE      = 5; // >=
	String[] ops = {"==","!=","<","<=",">",">="};
	private String _type  = ""; // Name of the node you want to check
	private int _operator = 0;  // type of condition
	private String _value = ""; // value of the condition
	
	public QLeaf(String tp, int op, String val) {
		this._type     = tp;
		this._operator = op;
		this._value    = val;
	}
	
	public String getType() {
		return this._type.toLowerCase();
	}
	
	public int getOperator() {
		return this._operator;
	}
	
	public String getValue() {
		return this._value.toLowerCase();
	}
    
    public String toString() {
        return "Leaf(" + getType() + ", " + ops[getOperator()] + ", " + getValue() + ")";
    }
	
}
