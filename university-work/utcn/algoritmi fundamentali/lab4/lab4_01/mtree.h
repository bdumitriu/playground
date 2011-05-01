/*
 * Header for the ModifiedTree class.
 *
 * Author:  Bogdan DUMITRIU
 * Date:    19.11.2001
 */

#ifndef __MODIFIED_TREE_H_
#define __MODIFIED_TREE_H_

#include <stdio.h>
#include <conio.h>

class Node
{
public:
	/*
	 * Builds a new node with the specified key and, optionally
	 * the specified parent, left and right node.
	 */
	Node(int key, Node *parent = NULL, Node *left = NULL,
		Node *right = NULL, Node *succ = NULL, Node *pred = NULL,
		Node *spNode = NULL);

	/*
	 * Destroys the node.
	 */
	~Node();

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
	void setParent(Node *parent);

	/*
	 * Returns the node's parent.
	 */
	Node* getParent() const;

	/*
	 * Sets the node's left child to left.
	 */
	void setLeft(Node *left);

	/*
	 * Returns the node's left child.
	 */
	Node* getLeft() const;

	/*
	 * Sets the node's right child to right.
	 */
	void setRight(Node *right);

	/*
	 * Returns the node's right child.
	 */
	Node* getRight() const;

	/*
	 * Sets the node's predecessor to pred.
	 */
	void setPredecessor(Node *pred);

	/*
	 * Returns the node's predecessor.
	 */
	Node* getPredecessor() const;

	/*
	 * Sets the node's successor to succ.
	 */
	void setSuccessor(Node *succ);

	/*
	 * Returns the node's successor.
	 */
	Node* getSuccessor() const;

	/*
	 * Sets the node's special parent to sp_parent.
	 */
	void setSpecialParent(Node *spParent);

	/*
	 * Returns the node's special parent.
	 */
	Node* getSpecialParent() const;

private:
	// node's key;
	int key;

	// node's parent
	Node *parent;

	// node's left child
	Node *left;

	// node's right child
	Node *right;

	// node's predecessor
	Node *pred;

	// node's successor
	Node *succ;

	/*
	 * a special pointer which is defined as follows:
	 *
	 *  - if the node is a left child then this pointer is to
	 *    the first node that is reached by following a right
	 *    parent-son link, thus:
	 *
	 *          (a)
	 *            \
	 *            (b)
	 *            /
	 *          (c)
	 *          /
	 *        (d)
	 *
	 *    in the picture above, both nodes c and d will point
	 *    to node a with this special pointer.
	 *
	 *  - if the node is a right child then this pointer is to
	 *    the first node that is reached by following a left
	 *    parent-son link, thus:
	 *
	 *          (a)
	 *          /
	 *        (b)
	 *          \
	 *          (c)
	 *            \
	 *            (d)
	 *
	 *    in the picture above, both nodes c and d will point
	 *    to node a with this special pointer.
	 *
	 * Since the nodes on the left most branch and the right most
	 * branch would have nothing to point to a special node will
	 * have to be created in a tree, a node which should root's
	 * parent and with root as its left and right child.
	 */
	Node *spParent;
};

class ModifiedTree
{
public:
	/*
	 * Builds a new tree.
	 */
	ModifiedTree();

	/*
	 * Destroys the node.
	 */
	~ModifiedTree();

	/*
	 * Adds newNode to the tree. The key field of newNode
	 * should be properly initialized.
	 */
	void addNode(Node *newNode);

	/*
	 * Removes node from the tree and returns a pointer to the
	 * actual Node to be deleted.
	 */
	Node* deleteNode(Node *node);

	/*
	 * Searches the subtree with rootNode as its root for a
	 * node with key as its key. If such a node exists, then
	 * a pointer to it is returned. Otherwise, NULL is returned.
	 * To search the entire tree, call findNode with
	 * treeObject->getRoot() as its second parameter.
	 */
	Node* findNode(int key, Node *rootNode);

	/*
	 * Returns the node with the minimum key value from the
	 * subtree that has rootNode as its root.
	 */
	Node* minNode(Node *rootNode);

	/*
	 * Returns the node with the maximum key value from the
	 * subtree that has rootNode as its root.
	 */
	Node* maxNode(Node *rootNode);

	/*
	 * Returns the node with the smallest key greater than
	 * the one of currentNode or NULL, if currentNode has
	 * the greatest key in the tree.
	 */
	Node* successor(const Node *currentNode);

	 /*
	 * Returns the node with the greatest key smaller than
	 * the one of currentNode or NULL, if currentNode has
	 * the smallest key in the tree.
	 */
	Node* predecessor(const Node *currentNode);

	/*
	 * Returns this tree's root.
	 */
	Node* getRoot() const;

	/*
	 * Sets the tree's root to root.
	 */
	void setRoot(Node *root);

	/*
	 * Prints a list containing all the keys of the subtree with
	 * rootNode as root.
	 */
	void printKeys(const Node *rootNode);

	/*
	 * Prints a pretty representation of the tree.
	 */
	void prettyPrint(const Node *rootNode);

private:
	void internalPrettyPrint(const Node *x, int depth);

	Node *spNode;
	Node *root;
};

inline int Node::getKey() const
{
	return key;
}

inline void Node::setKey(int key)
{
	this->key = key;
}

inline Node* Node::getParent() const
{
	return parent;
}

inline void Node::setParent(Node *parent)
{
	this->parent = parent;
}

inline Node* Node::getLeft() const
{
	return left;
}

inline void Node::setLeft(Node *left)
{
	this->left = left;
}

inline Node* Node::getRight() const
{
	return right;
}

inline void Node::setRight(Node *right)
{
	this->right = right;
}

inline Node* Node::getPredecessor() const
{
	return pred;
}

inline void Node::setPredecessor(Node *pred)
{
	this->pred = pred;
}

inline Node* Node::getSuccessor() const
{
	return succ;
}

inline void Node::setSuccessor(Node *succ)
{
	this->succ = succ;
}

inline Node* Node::getSpecialParent() const
{
	return spParent;
}

inline void Node::setSpecialParent(Node *spParent)
{
	this->spParent = spParent;
}

inline Node* ModifiedTree::getRoot() const
{
	return root;
}

inline void ModifiedTree::setRoot(Node *root)
{
	this->root = root;
}

#endif