/*
 * Header for the Red/Black Tree class.
 *
 * Author:  Bogdan DUMITRIU
 * Date:    26.11.2001
 */

#ifndef __RBTREE_H_
#define __RBTREE_H_

#include <stdio.h>
#include <conio.h>

/*
 * BLK for black & RD for red. They had to have other names
 * as we already have BLACK & RED in graphics.h under Borland C++.
 */
enum color {BLK = 0, RD = 1};

class RBNode
{
public:
	/*
	 * Builds a new node with the specified key and, optionally
	 * the specified parent, left node, right node and color.
	 */
	RBNode(int key, RBNode *parent = NULL, RBNode *left = NULL,
		RBNode *right = NULL, color col = RD);

	/*
	 * Destroys the node.
	 */
	~RBNode();

	/*
	 * Sets the node's key to key.
	 */
	void setKey(int key);

	/*
	 * Returns the node's key.
	 */
	int getKey() const;

	/*
	 * Sets the node's parent to parent.
	 */
	void setParent(RBNode *parent);

	/*
	 * Returns the node's parent.
	 */
	RBNode* getParent() const;

	/*
	 * Sets the node's left child to left.
	 */
	void setLeft(RBNode *left);

	/*
	 * Returns the node's left child.
	 */
	RBNode* getLeft() const;

	/*
	 * Sets the node's right child to right.
	 */
	void setRight(RBNode *right);

	/*
	 * Returns the node's right child.
	 */
	RBNode* getRight() const;

	/*
	 * Sets the node's color to col.
	 */
	void setColor(color col);

	/*
	 * Returns the node's color.
	 */
	color getColor() const;

private:
	// node's key;
	int key;

	// node's parent
	RBNode *parent;

	// node's left child
	RBNode *left;

	// node's right child
	RBNode *right;

	// node's color
	color col;
};

class RBTree
{
public:
	/*
	 * Builds a new red/black tree.
	 */
	RBTree();

	/*
	 * Destroys the node.
	 */
	~RBTree();

	/*
	 * Adds newNode to the red/black tree. The key field of newRBNode
	 * should be properly initialized.
	 */
	void addNode(RBNode *newNode);

	/*
	 * Removes node from the red/black tree and returns a pointer
	 * to the actual RBNode to be deleted.
	 */
	RBNode* deleteNode(RBNode *node);

	/*
	 * Searches the red/black subtree with rootNode as its root for a
	 * node with key as its key. If such a node exists, then
	 * a pointer to it is returned. Otherwise, NULL is returned.
	 * To search the entire red/black tree, call findNode with
	 * treeObject->getRoot() as its second parameter.
	 */
	RBNode* findNode(int key, RBNode *rootNode);

	/*
	 * Returns the node with the minimum key value from the
	 * red/black subtree that has rootNode as its root.
	 */
	RBNode* minNode(RBNode *rootNode);

	/*
	 * Returns the node with the maximum key value from the
	 * red/black subtree that has rootNode as its root.
	 */
	RBNode* maxNode(RBNode *rootNode);

	/*
	 * Returns the node with the smallest key greater than
	 * the one of currentNode or NULL, if currentNode has
	 * the greatest key in the red/black tree.
	 */
	RBNode* successor(const RBNode *currentNode);

	/*
	 * Returns the node with the greatest key smaller than
	 * the one of currentNode or NULL, if currentNode has
	 * the smallest key in the red/black tree.
	 */
	RBNode* predecessor(const RBNode *currentNode);

	/*
	 * Prints a list containing all the keys of the red/black
	 * subtree with rootNode as root.
	 */
	void printKeys(const RBNode *rootNode);

	/*
	 * Prints an indented image containing all the keys of
	 * the red/black subtree with rootNode as root.
	 */
	void prettyPrint(const RBNode *rootNode);

	/*
	 * Returns this red/black tree's root.
	 */
	RBNode* getRoot() const;

	/*
	 * Sets the red/black tree's root to root.
	 */
	void setRoot(RBNode *root);

private:
	RBNode *nil;
	RBNode *root;

	/*
	 * Makes a right rotation. That means:
	 *            (x)                         (y)
	 *           /   \                       /   \
	 *         (y) (gamma)   becomes:   (alpha)  (x)
	 *        /   \                             /   \
	 *   (alpha) (beta)                     (beta) (gamma)
	 */
	void rotateRight(RBNode *x);

	/*
	 * Makes a left rotation. That means:
	 *        (x)                                (y)
	 *       /   \                              /   \
	 *  (alpha)  (y)         becomes:         (x) (gamma)
	 *          /   \                        /   \
	 *      (beta) (gamma)              (alpha) (beta)
	 */
	void rotateLeft(RBNode *x);

	/*
	 * Restores the RB Tree properties after a normal search tree
	 * insertion of node x.
	 */
	void insertRB(RBNode *x);

	/*
	 * Restores the RB Tree properties after a normal search tree
	 * deletion. The node with an extra black color is x.
	 */
	void deleteRB(RBNode *x);

	/*
	 * It does the actual work for public prettyPrint function
	 * declared above. The only reason for having two functions
	 * is the extra depth parameter needed.
	 */
	void internalPrettyPrint(const RBNode *x, int depth);

	/*
	 * Frees the memory occupied by the RB subtree with
	 * x as root.
	 */
	void treeDelete(RBNode *x);
};

inline int RBNode::getKey() const
{
	return key;
}

inline void RBNode::setKey(int key)
{
	this->key = key;
}

inline RBNode* RBNode::getParent() const
{
	return parent;
}

inline void RBNode::setParent(RBNode *parent)
{
	this->parent = parent;
}

inline RBNode* RBNode::getLeft() const
{
	return left;
}

inline void RBNode::setLeft(RBNode *left)
{
	this->left = left;
}

inline RBNode* RBNode::getRight() const
{
	return right;
}

inline void RBNode::setRight(RBNode *right)
{
	this->right = right;
}

inline color RBNode::getColor() const
{
	return col;
}

inline void RBNode::setColor(color col)
{
	this->col = col;
}

inline RBNode* RBTree::getRoot() const
{
	return root;
}

inline void RBTree::setRoot(RBNode *root)
{
	this->root = root;
}

#endif