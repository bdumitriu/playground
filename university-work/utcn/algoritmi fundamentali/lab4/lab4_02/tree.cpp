/*
 * Implementation of the Tree class.
 *
 * Author:  Bogdan DUMITRIU
 * Date:    27.11.2001
 */


#include "tree.h"

Tree::Tree()
{
	root = NULL;
}

void treeDelete(Node *x)
{
	if (x != NULL)
	{
		treeDelete(x->getLeft());
		treeDelete(x->getRight());
		delete x;
	}
}

Tree::~Tree()
{
	treeDelete(root);
}

Node* Tree::minNode(Node *x)
{
	if (x == NULL)
		return NULL;
	while (x->getLeft() != NULL)
		x = x->getLeft();
	return x;
}

Node* Tree::maxNode(Node *x)
{
	if (x == NULL)
		return NULL;
	while (x->getRight() != NULL)
		x = x->getRight();
	return x;
}

Node* Tree::successor(const Node *x)
{
	if (x->getRight() != NULL)
		return minNode(x->getRight());
	Node *y = x->getParent();
	while ((y != NULL) && (x == y->getRight()))
	{
		x = y;
		y = x->getParent();
	}
	return y;
}

Node* Tree::predecessor(const Node *x)
{
	if (x->getLeft() != NULL)
		return maxNode(x->getLeft());
	Node *y = x->getParent();
	while ((y != NULL) && (x == y->getLeft()))
	{
		x = y;
		y = x->getParent();
	}
	return y;
}

void Tree::addNode(Node *x)
{
	Node *y = NULL;
	Node *z = root;
	x->setLeft(NULL);
	x->setRight(NULL);
	while (z != NULL)
	{
		y = z;
		if (x->getKey() < z->getKey())
			z = z->getLeft();
		else
			z = z->getRight();
	}
	x->setParent(y);
	if (y == NULL)
		root = x;
	else
		if (x->getKey() < y->getKey())
			y->setLeft(x);
		else
			y->setRight(x);
}

Node* Tree::findNode(int key, Node *x)
{
	while ((x != NULL) && (x->getKey() != key))
	{
		if (key < x->getKey())
			x = x->getLeft();
		else
			x = x->getRight();
	}
	return x;
}

Node* Tree::findSmallestNodeGreaterThan(int key, Node *x)
{
	if (x == NULL)
		return NULL;

	Node *s_root = x;
	int gata = 0;

	while (!gata)
		if (key == x->getKey())
			gata = 1;
		else
			if (key < x->getKey())
				if (x->getLeft() != NULL)
					x = x->getLeft();
				else
					gata = 1;
			else
				if (x->getRight() != NULL)
					x = x->getRight();
				else
					gata = 2;

	if (gata == 2)
	{
		while ((x != s_root) && (x == x->getParent()->getRight()))
			x = x->getParent();
		if (x != s_root)
			x = x->getParent();
		else
			return NULL;
	}

	return x;
}


Node* Tree::deleteNode(Node *x)
{
	Node *y, *z;

	if (x == NULL)
		return NULL;

	// see which case we are dealing with
	if ((x->getLeft() == NULL) || (x->getRight() == NULL))
		y = x;
	else
		y = successor(x);

	// set z to the non-NULL son of y
	// (y only has 1 non-NULL son after the code above)
	if (y->getLeft() != NULL)
		z = y->getLeft();
	else
		z = y->getRight();

	// set z's parent
	if (z != NULL)
		z->setParent(y->getParent());

	// set the links in order
	if (y->getParent() == NULL)
		root = z;
	else
		if (y == y->getParent()->getLeft())
			y->getParent()->setLeft(z);
		else
			y->getParent()->setRight(z);

	// move data if necessary
	if (y != x)
		x->setKey(y->getKey());
	return y;
}

void Tree::printKeys(const Node *x)
{
	if (x != NULL)
	{
		printKeys(x->getLeft());
		printf("%d ", x->getKey());
		printKeys(x->getRight());
	}
}


void Tree::printKeysBetween(int a, int b, Node *x)
{
	if (x != NULL)
	{
		if (x->getKey() <= a)
		{
			if (x->getKey() == a)
				printf("%d ", x->getKey());
			printKeysBetween(a, b, x->getRight());
		}
		else
			if (x->getKey() >= b)
			{
				printKeysBetween(a, b, x->getLeft());
				if (x->getKey() == b)
					printf("%d ", x->getKey());
			}
			else
			{
				printKeysBetween(a, b, x->getLeft());
				printf("%d ", x->getKey());
				printKeysBetween(a, b, x->getRight());
			}

	}
}