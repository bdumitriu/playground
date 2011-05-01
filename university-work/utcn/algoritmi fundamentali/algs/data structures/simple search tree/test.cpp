#include <stdio.h>
#include <conio.h>
#include "tree.h"

void main()
{
	Tree *t = new Tree();

	clrscr();
	t->addNode(new Node(20));
	t->addNode(new Node(11));
	t->addNode(new Node(28));
	t->addNode(new Node(17));
	t->addNode(new Node(27));
	t->addNode(new Node(5));
	t->addNode(new Node(12));
	t->addNode(new Node(11));
	t->addNode(new Node(16));

	int c;
	do
	{
		t->printKeys(t->getRoot());
		printf("\n nod: ");
		scanf("%d", &c);
		t->deleteNode(t->findNode(c, t->getRoot()));
	}
	while (c != 0);

	delete t;
}