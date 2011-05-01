/*
 * Implementation for the Red/Black Tree class.
 *
 * Author:  Bogdan DUMITRIU
 * Date:    26.11.2001
 */

#include "rbtree.h"

RBTree::RBTree()
{
	// it is vital that nil's color is set to BLK
	nil = new RBNode(0, NULL, NULL, NULL, BLK);
	root = nil;
}

void RBTree::treeDelete(RBNode *x)
{
	if (x != nil)
	{
		treeDelete(x->getLeft());
		treeDelete(x->getRight());
		delete x;
	}
}

RBTree::~RBTree()
{
	treeDelete(root);
	delete nil;
}

void RBTree::rotateRight(RBNode *x)
{
	RBNode *y = x->getLeft();

	if ((y == nil) || (y == NULL))
		return;

	x->setLeft(y->getRight());
	if (y->getRight() != nil)
		y->getRight()->setParent(x);
	y->setParent(x->getParent());
	if (x->getParent() == nil)
		root = y;
	else
		if (x == x->getParent()->getLeft())
			x->getParent()->setLeft(y);
		else
			x->getParent()->setRight(y);
	y->setRight(x);
	x->setParent(y);
}

void RBTree::rotateLeft(RBNode *x)
{
	RBNode *y = x->getRight();

	if ((y == nil) || (y == NULL))
		return;

	x->setRight(y->getLeft());
	if (y->getLeft() != nil)
		y->getLeft()->setParent(x);
	y->setParent(x->getParent());
	if (x->getParent() == nil)
		root = y;
	else
		if (x == x->getParent()->getLeft())
			x->getParent()->setLeft(y);
		else
			x->getParent()->setRight(y);
	y->setLeft(x);
	x->setParent(y);
}

RBNode* RBTree::minNode(RBNode *x)
{
	if ((x == NULL) || (x == nil))
		return NULL;

	while (x->getLeft() != nil)
		x = x->getLeft();

	return x;
}

RBNode* RBTree::maxNode(RBNode *x)
{
	if ((x == NULL) || (x == nil))
		return NULL;

	while (x->getRight() != nil)
		x = x->getRight();

	return x;
}

RBNode* RBTree::successor(const RBNode *x)
{
	if ((x == NULL) || (x == nil))
		return NULL;

	// if x has a right subtree than its successor
	// is the minimum value of that subtree
	if (x->getRight() != nil)
		return minNode(x->getRight());

	// otherwise, we go up as long as we are a right
	// child. Once we become a left child, we have found
	// our successor. If we never become a left child
	// than we're the greatest in the tree, so NULL is
	// returned.
	RBNode *y = x->getParent();
	while ((y != nil) && (x == y->getRight()))
	{
		x = y;
		y = x->getParent();
	}

	if (y != nil)
		return y;
	else
		return NULL;
}

RBNode* RBTree::predecessor(const RBNode *x)
{
	if ((x == NULL) || (x == nil))
		return NULL;

	// if x has a left subtree than its predecessor
	// is the maximum value of that subtree
	if (x->getLeft() != nil)
		return maxNode(x->getLeft());

	// otherwise, we go up as long as we are a left
	// child. Once we become a right child, we have found
	// our predecessor. If we never become a right child
	// than we're the smallest in the tree, so NULL is
	// returned.
	RBNode *y = x->getParent();
	while ((y != nil) && (x == y->getLeft()))
	{
		x = y;
		y = x->getParent();
	}

	if (y != nil)
		return y;
	else
		return NULL;
}

/*
 * For details about the code used by this function,
 * the book "Introduction to Algorithms", by T.H. Cormen,
 * C.E. Leiserson & R.R. Rivest can be an extremely
 * valuable source of information.
 */
void RBTree::insertRB(RBNode *x)
{
	RBNode *y;

	x->setColor(RD);
	while ((x != root) && (x->getParent()->getColor() == RD))
	{
		if (x->getParent() == x->getParent()->getParent()->getLeft())
		{
			y = x->getParent()->getParent()->getRight();
			if (y->getColor() == RD)
			{
				y->setColor(BLK);
				x->getParent()->setColor(BLK);
				x->getParent()->getParent()->setColor(RD);
				x = x->getParent()->getParent();
			}
			else
			{
				if (x == x->getParent()->getRight())
				{
					x = x->getParent();
					rotateLeft(x);
				}
				x->getParent()->setColor(BLK);
				x->getParent()->getParent()->setColor(RD);
				rotateRight(x->getParent()->getParent());
			}
		}
		else
		{
			y = x->getParent()->getParent()->getLeft();
			if (y->getColor() == RD)
			{
				y->setColor(BLK);
				x->getParent()->setColor(BLK);
				x->getParent()->getParent()->setColor(RD);
				x = x->getParent()->getParent();
			}
			else
			{
				if (x == x->getParent()->getLeft())
				{
					x = x->getParent();
					rotateRight(x);
				}
				x->getParent()->setColor(BLK);
				x->getParent()->getParent()->setColor(RD);
				rotateLeft(x->getParent()->getParent());
			}
		}
	}
	root->setColor(BLK);
}

void RBTree::addNode(RBNode *x)
{
	if ((x == NULL) || (x == nil))
		return;

	x->setLeft(nil);
	x->setRight(nil);

	// look for x's future parent (it will be y)
	RBNode *y = nil;
	RBNode *z = root;
	while (z != nil)
	{
		y = z;
		if (x->getKey() < z->getKey())
			z = z->getLeft();
		else
			z = z->getRight();
	}

	// set x's parent and update root/x's left/x's right if necessary
	x->setParent(y);
	if (y == nil)
		root = x;
	else
		if (x->getKey() < y->getKey())
			y->setLeft(x);
		else
			y->setRight(x);

	// make sure our tree stays a RB Tree
	insertRB(x);
}

RBNode* RBTree::findNode(int key, RBNode *x)
{
	if ((x == NULL) || (x == nil))
		return NULL;

	while ((x != nil) && (x->getKey() != key))
	{
		if (key < x->getKey())
			x = x->getLeft();
		else
			x = x->getRight();
	}

	if (x != nil)
		return x;
	else
		return NULL;
}

/*
 * For details about the code used by this function,
 * the book "Introduction to Algorithms", by T.H. Cormen,
 * C.E. Leiserson & R.R. Rivest can be an extremely
 * valuable source of information.
 */
void RBTree::deleteRB(RBNode *x)
{
	RBNode *w;

	while ((x != root) && (x->getColor() == BLK))
	{
		if (x == x->getParent()->getLeft())
		{
			w = x->getParent()->getRight();
			if (w->getColor() == RD)
			{
				w->setColor(BLK);
				x->getParent()->setColor(RD);
				rotateLeft(x->getParent());
				w = x->getParent()->getRight();
			}
			if ((w->getLeft()->getColor() == BLK) &&
				(w->getRight()->getColor() == BLK))
			{
				w->setColor(RD);
				x = x->getParent();
			}
			else
			{
				if (w->getRight() == BLK)
				{
					w->getLeft()->setColor(BLK);
					w->setColor(RD);
					rotateRight(w);
					w = x->getParent()->getRight();
				}
				w->setColor(x->getParent()->getColor());
				x->getParent()->setColor(BLK);
				w->getRight()->setColor(BLK);
				rotateLeft(x->getParent());
				x = root;
			}
		}
		else
		{
			w = x->getParent()->getLeft();
			if (w->getColor() == RD)
			{
				w->setColor(BLK);
				x->getParent()->setColor(RD);
				rotateRight(x->getParent());
				w = x->getParent()->getLeft();
			}
			if ((w->getRight()->getColor() == BLK) &&
				(w->getLeft()->getColor() == BLK))
			{
				w->setColor(RD);
				x = x->getParent();
			}
			else
			{
				if (w->getLeft() == BLK)
				{
					w->getRight()->setColor(BLK);
					w->setColor(RD);
					rotateLeft(w);
					w = x->getParent()->getLeft();
				}
				w->setColor(x->getParent()->getColor());
				x->getParent()->setColor(BLK);
				w->getLeft()->setColor(BLK);
				rotateRight(x->getParent());
				x = root;
			}
		}
	}
	x->setColor(BLK);
}

RBNode* RBTree::deleteNode(RBNode *x)
{
	RBNode *y, *z;

	if ((x == NULL) || (x == nil))
		return NULL;

	// see which case we are dealing with
	// after this, y will be the physical node to delete
	if ((x->getLeft() == nil) || (x->getRight() == nil))
		y = x;
	else
		y = successor(x);

	// set z to the non-nil son of y
	// (y has at most a non-nil son after the code above)
	if (y->getLeft() != nil)
		z = y->getLeft();
	else
		z = y->getRight();

	// set z's parent (we still need this even for z=nil
	// for the deleteRB function to work fine).
	z->setParent(y->getParent());

	// update the links
	if (y->getParent() == nil)
		root = z;
	else
		if (y == y->getParent()->getLeft())
			y->getParent()->setLeft(z);
		else
			y->getParent()->setRight(z);

	// move data if necessary
	if (y != x)
		x->setKey(y->getKey());

	if (y->getColor() == BLK)
		deleteRB(z);
	return y;
}

void RBTree::printKeys(const RBNode *x)
{
	if (x == NULL)
		return;

	if (x != nil)
	{
		printKeys(x->getLeft());
		if (x->getColor() == RD)
			printf("%d(red) ", x->getKey());
		else
			printf("%d(black) ", x->getKey());
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

void RBTree::internalPrettyPrint(const RBNode *x, int depth)
{
	if (x == NULL)
		return;

	if (x != nil)
	{
		internalPrettyPrint(x->getRight(), depth+1);
		printSpaces(depth);
		if (x->getColor() == RD)
			printf("%d(r)\n", x->getKey());
		else
			printf("%d(b)\n", x->getKey());
		internalPrettyPrint(x->getLeft(), depth+1);
	}
}

void RBTree::prettyPrint(const RBNode *x)
{
	internalPrettyPrint(x, 0);
}