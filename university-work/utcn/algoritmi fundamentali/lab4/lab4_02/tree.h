/*
 * Header for the Tree class.
 *
 * Author:  Bogdan DUMITRIU
 * Date:    19.11.2001
 */

#ifndef __TREE_H_
#define __TREE_H_

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
		Node *right = NULL);

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

private:
	// node's key;
	int key;

	// node's parent
	Node *parent;

	// node's left child
	Node *left;

	// node's right child
	Node *right;
};

class Tree
{
public:
	/*
	 * Builds a new tree.
	 */
	Tree();

	/*
	 * Destroys the node.
	 */
	~Tree();

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
	 * Searches the subtree with rootNode as its root for the node
	 * with the smallest key that satisfies node_key >= key. If such
	 * a node exists, a pointer to it is returned. Otherwise, NULL
	 * is returned. To search the entire tree, call
	 * findSmallestKeyGreaterThan with treeObject->getRoot() as
	 * its second parameter.
	 */
	Node* findSmallestNodeGreaterThan(int key, Node *rootNode);

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
	 * Prints a list containing all the keys of the subtree with
	 * rootNode as root.
	 */
	void printKeys(const Node *rootNode);

	/*
	 * Prints a list containing all the keys of the subtree with
	 * rootNode as its root that are between a and b.
	 */
	void printKeysBetween(int a, int b, Node *rootNode);

	/*
	 * Sets the tree's root to root.
	 */
	void setRoot(Node *root);

private:
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

inline Node* Tree::getRoot() const
{
	return root;
}

inline void Tree::setRoot(Node *root)
{
	this->root = root;
}

#endif