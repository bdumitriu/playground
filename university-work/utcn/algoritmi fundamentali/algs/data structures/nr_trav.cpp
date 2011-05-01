/*
 * This algorithm non-recursively traverses a tree in an O(n) time.
 *
 * Autor: Bogdan DUMITRIU
 * Data:  12.11.2001
 */

#include <stdio.h>
#include <conio.h>
#include <alloc.h>

struct node
{
	node *p, *l, *r;
	int key;
};

/*
 * Reads a tree from the user.
 */
node* read_tree(node *parent)
{
	int key;
	node *newnode;
	printf(" key (0 for null): ");
	scanf("%d", &key);
	if (key != 0)
	{
		newnode = (node*) malloc(sizeof(struct node));
		newnode->p = parent;
		newnode->key = key;
		newnode->l = read_tree(newnode);
		newnode->r = read_tree(newnode);
		return newnode;
	}
	else
		return NULL;
}

/*
 * Prints the tree recursively in in-order.
 */
void print_tree(node *root)
{
	if (root != NULL)
	{
		printf("%d ", root->key);
		print_tree(root->l);
		print_tree(root->r);
	}
}

/*
 * The actual non-recursive traverse of the tree.
 */
void traverse_tree(node *root)
{
	node *r = NULL;
	node *p = root;

	while (p != NULL)
		if (r == p->p)
		{
			printf("%d ", p->key);
			r = p;
			if (p->l != NULL)
				p = p->l;
			else
				if (p->r != NULL)
					p = p->r;
				else
					p = p->p;
		}
		else
			if (r == p->l)
			{
				r = p;
				if (p->r != NULL)
					p = p->r;
				else
					p = p->p;
			}
			else
			{
				r = p;
				p = p->p;
			}
}

void main()
{
	node *root;

	clrscr();
	root = read_tree(NULL);
	printf("The tree nodes are (in in-order): ");
	print_tree(root);
	printf("\nThe tree nodes (again) are (in in-order): ");
	traverse_tree(root);
	printf("\n");
	getch();
}