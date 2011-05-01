/*
 * Implementation of the ModifiedTree class.
 *
 * Author: Bogdan DUMITRIU
 * Date:   27.12.2001
 */

#include "mtree.h"

ModifiedTree::ModifiedTree()
{
	root = NULL;
    spNode = new Node(0);
}

/*
 * Deletes the entire tree with root x.
 */
void treeDelete(Node *x)
{
	if (x != NULL)
	{
		treeDelete(x->getLeft());
		treeDelete(x->getRight());
		delete x;
	}
}

ModifiedTree::~ModifiedTree()
{
	treeDelete(root);
}

Node* ModifiedTree::minNode(Node *x)
{
	if (x == NULL)
		return NULL;
	if (x->getParent()->getLeft() == x)
		return x->getSpecialParent()->getSuccessor();
	else
		return x->getParent()->getSuccessor();
}

Node* ModifiedTree::maxNode(Node *x)
{
	if (x == NULL)
		return NULL;
	if (x->getParent()->getLeft() == x)
		return x->getParent()->getPredecessor();
	else
		return x->getSpecialParent()->getPredecessor();
}

Node* ModifiedTree::successor(const Node *x)
{
	if (x == NULL)
    	return NULL;
	return x->getSuccessor();
}

Node* ModifiedTree::predecessor(const Node *x)
{
	if (x == NULL)
		return NULL;
	return x->getPredecessor();
}

void ModifiedTree::addNode(Node *x)
{
	Node *z = root;
	Node *y = NULL;
	Node *s = NULL;
	Node *p = NULL;

	x->setLeft(NULL);
	x->setRight(NULL);

	while (z != NULL)
	{
		y = z;
		if (x->getKey() < z->getKey())
		{
			s = z;
			z = z->getLeft();
		}
		else
		{
			p = z;
			z = z->getRight();
		}
	}
	x->setParent(y);

	// set the predecessor/successor links
	x->setSuccessor(s);
	x->setPredecessor(p);
	if (p != NULL)
		p->setSuccessor(x);
	else
		spNode->setSuccessor(x);
	if (s != NULL)
		s->setPredecessor(x);
	else
		spNode->setPredecessor(x);

	if (y == NULL)
	{
		root = x;
		root->setParent(spNode);
		root->setSpecialParent(spNode);
		spNode->setLeft(root);
		spNode->setRight(root);
		spNode->setPredecessor(root);
		spNode->setSuccessor(root);
	}
	else
	{
		if (x->getKey() < y->getKey())
			y->setLeft(x);
		else
			y->setRight(x);

		// set the spParent link
		int nLinkType;
		int pLinkType;

		if (y->getLeft() == x)
			nLinkType = 1;
		else
			nLinkType = 2;
		if (y->getParent()->getLeft() == y)
			pLinkType = 1;
		else
			pLinkType = 2;

		if (nLinkType == pLinkType)
			x->setSpecialParent(y->getSpecialParent());
		else
			x->setSpecialParent(y->getParent());
	}
}

Node* ModifiedTree::findNode(int key, Node *x)
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

Node* ModifiedTree::deleteNode(Node *x)
{
	Node *y, *z;

	if (x == NULL)
		return NULL;

	// see which case we are dealing with
	if ((x->getLeft() == NULL) || (x->getRight() == NULL))
		y = x;
	else
		y = successor(x);

	// set z to the non-NULL son of y (if it has one) or to NULL.
	// (y only has a maximum of 1 non-NULL son after the code above)
	if (y->getLeft() != NULL)
		z = y->getLeft();
	else
		z = y->getRight();

	// update spParent links
	if ((z == y->getLeft()) && (y == y->getParent()->getRight()))
	{
		Node *temp = z;
		Node *sp = y->getSpecialParent();
		while (temp != NULL)
		{
			temp->setSpecialParent(sp);
			temp = temp->getRight();
		}
	}
	else
		if ((z == y->getRight()) && (y == y->getParent()->getLeft()))
		{
			Node *temp = z;
			Node *sp = y->getSpecialParent();
			while (temp != NULL)
			{
				temp->setSpecialParent(sp);
				temp = temp->getLeft();
			}
		}

	// set z's parent
	if (z != NULL)
		z->setParent(y->getParent());

	// update the left/right child links
	if (y->getParent() == spNode)
	{
		root = z;
		if (z == NULL)
		{
			spNode->setLeft(NULL);
			spNode->setRight(NULL);
			spNode->setPredecessor(NULL);
			spNode->setSuccessor(NULL);
		}
	}
	else
		if (y == y->getParent()->getLeft())
			y->getParent()->setLeft(z);
		else
			y->getParent()->setRight(z);

	// move data if necessary
	if (y != x)
		x->setKey(y->getKey());

	// update predecessor/successor links
	if (y->getSuccessor() != NULL)
		y->getSuccessor()->setPredecessor(y->getPredecessor());
	else
		spNode->setPredecessor(y->getPredecessor());
	if (y->getPredecessor() != NULL)
		y->getPredecessor()->setSuccessor(y->getSuccessor());
	else
		spNode->setSuccessor(y->getSuccessor());
	y->setSuccessor(NULL);
	y->setPredecessor(NULL);

	return y;
}

void ModifiedTree::printKeys(const Node *x)
{
	if (x != NULL)
	{
		printKeys(x->getLeft());
		printf("%d ", x->getKey());
		printKeys(x->getRight());
	}
}

/*
 * Prints mult*8 spaces.
 */
void printSpaces(int mult)
{
	for (int i = 0; i < mult*8; i++)
		printf(" ");
}

void ModifiedTree::internalPrettyPrint(const Node *x, int depth)
{
	if (x == NULL)
		return;

	if (x != NULL)
	{
		internalPrettyPrint(x->getRight(), depth+1);
		printSpaces(depth);
		printf("%d\n", x->getKey());
		internalPrettyPrint(x->getLeft(), depth+1);
	}
}

void ModifiedTree::prettyPrint(const Node *x)
{
	internalPrettyPrint(x, 0);
}