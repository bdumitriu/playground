/*
 * A small application for testing the functionality of the
 * Red/Black Tree class.
 *
 * Author: Bogdan DUMITRIU
 * Date:   26.11.2001
 */

#include <stdio.h>
#include <conio.h>
#include "rbtree.h"

/*
 * Prints a list of commands on the screen.
 */
void print_commands()
{
	clrscr();
	printf("\ni - insert a node in the red/black tree.");
	printf("\ns - search for a node in the red/black tree.");
	printf("\nd - delete a node from the red/black tree.");
	printf("\np - print the red/black tree.");
	printf("\nESC - end program.\n\n");
	printf("Your command: ");
}

void main()
{
	RBTree *t = new RBTree();
	int n;
	RBNode *node;
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
				t->addNode(new RBNode(n));
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
					t->deleteNode(node);
					printf("Node %d deleted.", n);
				}
				break;
			case 'p':
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