package gw.query;

/**
 * Part of the tree datastructure to store the bibtex queries
 * If a node is of type SINGLE, it can only have one leaf element
 * If a node is of type AND or type OR, it has two or more subnodes
 */
public class QNode {
	final public static int SINGLE = 0; // TYPE OF NODE
	final public static int AND    = 1; // TYPE OF NODE
	final public static int OR     = 2; // TYPE OF NODE

	private int _type;
	//private QNode _parent     = null;  // Parent of this node
	private QNode _leftChild  = null;  // left child
	private QNode _rightChild = null;  // right child

	private QLeaf _leaf = null;        // A Leaf child in case this node is SINGLE.

	public QNode(int tp) {
		this._type = tp;
	}
    
    public boolean isOrNode() {
        return this._type == OR;
    }
    
    public boolean isAndNode() {
        return this._type == AND;
    }
    
     public boolean isSingle() {
        return this._type == 0;
    }
    int getType(){
        return this._type;
    }

	public void setLeaf(QLeaf leaf) {
		this._leaf = leaf;
	}

	public QLeaf getLeaf() {
		return this._leaf;
	}
	
	public QNode getLeftQNode()	{
        return this._leftChild;
    }
    public QNode getRightQNode()	{
        return this._rightChild;
    }
    
    public void setLeftQNode(QNode node)	{
        this._leftChild = node;
    }
    public void setRightQNode(QNode node)	{
        this._rightChild = node;
    }
	
	public String toString()
	{
        String m = "";
        if(getType() == OR)
            m = "["+this._leftChild.toString()+" OR "+this._rightChild.toString()+"]";
        else if(getType() == AND)
            m = "("+this._leftChild.toString()+" AND "+this._rightChild.toString()+")";
        else
            m = getLeaf().toString();
        return m;
    }
}
