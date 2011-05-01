package gw.query;

/**
 * Part of the tree datastructure to store the bibtex queries
 */
public class QTree {
	private QNode rootNode = null; // contains the root element 
			
	public QTree(QNode root) {
		this.rootNode = root;
	}

	public QNode getRootNode() {
		return this.rootNode;
	}	
}