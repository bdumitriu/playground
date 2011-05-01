/*
 * This program reads the elements of a tree and two limits
 * of an interval. It prints all tree nodes between the two
 * limits with an efficiency of O(m+lgn), where m represents
 * the number of tree nodes in the interval and n represents
 * the total number of nodes in the tree.
 *
 * Author: Bogdan DUMITRIU
 * Date:   22.12.2001
 */

#include <stdio.h>
#include <conio.h>
#include <stdlib.h>
#include "tree.h"

void print_values1(int a, int b, Tree *t)
{
	Node *x = t->findSmallestNodeGreaterThan(a, t->getRoot());

	if (x != NULL)
		while (x->getKey() <= b)
		{
			printf("%d ", x->getKey());
			x = t->successor(x);
		}
}

void print_values2(int a, int b, Tree *t)
{
	t->printKeysBetween(a, b, t->getRoot());
}

void main()
{
	Tree *t = new Tree();
	int a, b;

	clrscr();
	printf("Please type in the tree's nodes. Type value 0 to end.\n\n");

	int c;
	do
	{
		printf(" node value: ");
		scanf("%d", &c);
		if (c != 0)
			t->addNode(new Node(c));
	}
	while (c != 0);

	do
	{
		printf("\nInferior limit: ");
		scanf("%d", &a);
		printf("Superior limit: ");
		scanf("%d", &b);

		if (a > b)
			printf("\nError: inferior limit greater than superior limit.\n");
		else
		{
			printf("\nNodes bewteen %d and %d: ", a, b);
			print_values2(a, b, t);
			printf("\n");
		}
		printf("\nOnce again (y/n) ");
		c = getch();
		if (c == 0)
			getch();
		printf("%c\n", c);
	}
	while ((c == 'y') || (c == 'Y'));

	delete t;
}