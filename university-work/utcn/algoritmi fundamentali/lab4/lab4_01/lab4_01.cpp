/*
 * A small application for testing the functionality of the
 * ModifiedTree class.
 *
 * Author: Bogdan DUMITRIU
 * Date:   27.12.2001
 */

#include <stdio.h>
#include <conio.h>
#include "mtree.h"

/*
 * Prints a list of commands on the screen.
 */
void print_commands()
{
	clrscr();
	printf("\ni - insert a node in the tree.");
	printf("\ns - search for a node in the tree.");
	printf("\nd - delete a node from the tree.");
	printf("\nn - type the successor of a node.");
	printf("\np - type the predecessor of a node.");
	printf("\na - type the minimum value of a subtree.");
	printf("\nz - type the maximum value of a subtree.");
	printf("\nl - print the tree.");
	printf("\nESC - end program.\n\n");
	printf("Your command: ");
}

void main()
{
	ModifiedTree *t = new ModifiedTree();
	int n;
	Node *node;
	char c;

	do
	{
		print_commands();
		c = getch();
		if (c == 0)
			c = getch();
		if (c != 27)
			printf("%c\n\n", c);
		switch (c)
		{
			case 'i':
				printf("Node to insert: ");
				scanf("%d", &n);
				t->addNode(new Node(n));
				printf("Node inserted.");
				break;
			case 's':
				printf("Node to search for: ");
				scanf("%d", &n);
				if (t->findNode(n, t->getRoot()) == NULL)
					printf("Can't find node with key %d.", n);
				else
					printf("Found a node with key %d.", n);
				break;
			case 'd':
				printf("Node to delete: ");
				scanf("%d", &n);
				node = t->findNode(n, t->getRoot());
				if (node == NULL)
					printf("Can't find node with key %d.", n);
				else
				{
					delete t->deleteNode(node);
					printf("Node %d deleted.", n);
				}
				break;
			case 'n':
				printf("Find successor of: ");
				scanf("%d", &n);
				node = t->findNode(n, t->getRoot());
				if (node == NULL)
					printf("Can't find node with key %d.", n);
				else
					if (t->successor(node) != NULL)
						printf("Successor of %d is %d.", n,
							t->successor(node)->getKey());
					else
						printf("Node %d is greatest in tree.", n);
				break;
			case 'p':
				printf("Find predecessor of: ");
				scanf("%d", &n);
				node = t->findNode(n, t->getRoot());
				if (node == NULL)
					printf("Can't find node with key %d.", n);
				else
					if (t->predecessor(node) != NULL)
						printf("Predecessor of %d is %d.", n,
							t->predecessor(node)->getKey());
					else
						printf("Node %d is smallest in tree.", n);
				break;
			case 'a':
				printf("Find minimum value in subtree with root: ");
				scanf("%d", &n);
				node = t->findNode(n, t->getRoot());
				if (node == NULL)
					printf("Can't find node with key %d.", n);
				else
					printf("Minimum value is %d.",
						t->minNode(node)->getKey());
				break;
			case 'z':
				printf("Find maximum value in subtree with root: ");
				scanf("%d", &n);
				node = t->findNode(n, t->getRoot());
				if (node == NULL)
					printf("Can't find node with key %d.", n);
				else
					printf("Maximum value is %d.",
						t->maxNode(node)->getKey());
				break;
			case 'l':
				clrscr();
				printf("Tree is: \n\n");
				t->prettyPrint(t->getRoot());
				break;
			case 27:
				break;
			default:
				printf("Unknown command.");
				break;
		}
		if (c != 27)
		{
			printf("\n\nPress any key to continue.");
			if (getch() == 0)
				getch();
		}
	}
	while (c != 27);

	delete t;
}