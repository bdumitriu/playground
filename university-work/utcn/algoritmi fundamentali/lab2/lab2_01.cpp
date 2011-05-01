/*
 * Author: Bogdan DUMITRIU
 * Date:   23.11.2001
 *
 * This program allows manipulations of a double linked list
 * with one pointer only. This pointer is an exclusive or
 * between the addresses of the node's predecessor (0 if head)
 * and the node's successor (0 it tail). We use two pointers
 * to the first and last node and, in order to traverse the list
 * we use either of the following techniques:
 *  - if we know a node's predecessor then the node's successor will be:
 *		succ = pred xor (pred xor succ)
 *  - if we know a node's successor then the node's predecessor will be:
 *		pred = succ xor (pred xor succ)
 * where in both cases (pred xor succ) is the pointer stored in the
 * node.
 * Such a list can reversed in O(1) time by simply interchanging
 * its head with its tail.
 */

#include <stdio.h>
#include <conio.h>

struct node_structure
{
	int data;
	struct node_structure *p;
};

typedef struct node_structure node;

/*
 * The nil element of the list.
 */
node *nil = new node;

/*
 * The head & tail of the list.
 */
node *head = nil, *tail = nil;

/*
 * Although the two functions that follow are technically
 * identical, I have written them each for increased readability
 * of the manipulation functions.
 */

/*
 * Having both the node's address (p) and the node's successor's
 * address (succ) pred returns the node's predecessor's address.
 */
node* pred(node *p, node *succ)
{
	unsigned long n_addr = (unsigned long) p->p;
	unsigned long s_addr = (unsigned long) succ;
	unsigned long p_addr = (unsigned long) n_addr^s_addr; // in C, ^ = xor

	return (node *) p_addr;
}

/*
 * Having both the node's address (p) and the node's predecessor's
 * address (pred) succ returns the node's successor's address.
 */
node* succ(node *p, node *pred)
{
	unsigned long n_addr = (unsigned long) p->p;
	unsigned long p_addr = (unsigned long) pred;
	unsigned long s_addr = (unsigned long) n_addr^p_addr; // in C, ^ = xor

	return (node *) s_addr;
}

/*
 * Having two addresses, this function returns the address
 * that results by applying exclusive or on the two addresses.
 */
node* e_or(node *p1, node *p2)
{
	unsigned long p1_addr = (unsigned long) p1;
	unsigned long p2_addr = (unsigned long) p2;
	unsigned long p_addr = (unsigned long) p1_addr^p2_addr;

	return (node *) p_addr;
}

/*
 * This function expects an instantiated node with its data
 * field set. This node will be inserted at the beginning of
 * the list.
 */
void list_insert(node* new_elem)
{
	new_elem->p = e_or(nil, head);
	head->p = e_or(new_elem, succ(head, nil));
	head = new_elem;
	if (tail == nil)
		tail = new_elem;
}

/*
 * Returns the predecessor of element with data as its data if
 * it exists in the list and NULL otherwise.
 */
node* list_pred(int data)
{
	node *p = head;
	node *q = nil;
	node *temp;

	while (p != nil)
	{
		if (p->data == data)
			return q;
		temp = p;
		p = succ(p, q);
		q = temp;
	}

	return NULL;
}

/*
 * Returns the successor of element with data as its data if
 * it exists in the list and NULL otherwise.
 */
node* list_succ(int data)
{
	node *p = tail;
	node *q = nil;
	node *temp;

	while (p != nil)
	{
		if (p->data == data)
			return q;
		temp = p;
		p = pred(p, q);
		q = temp;
	}

	return NULL;
}

/*
 * Deletes the node pointed by elem_to_delete form the list.
 */
void list_delete(node *elem_to_delete)
{
	node *pr = list_pred(elem_to_delete->data);
	if (pr != NULL)
	{
		node *sc = succ(elem_to_delete, pr);

		if (pr != nil)
		{
			node *t = pred(pr, elem_to_delete);
			pr->p = e_or(t, sc);
		}
		else	// this means that elem_to_delete is the first element
			head = sc;

		if (sc != nil)
		{
			node *t = succ(sc, elem_to_delete);
			sc->p = e_or(t, pr);
		}
		else	// this means that elem_to_delete is the last element
			tail = pr;
	}
}

/*
 * Returns a pointer to the first element in the list that has
 * the data equal to data. If no such element in found NULL is
 * returned.
 */
node* list_search(int data)
{
	node *p = head;
	node *q = nil;
	node *temp;

	while (p != nil)
	{
		if (p->data == data)
			return p;
		temp = p;
		p = succ(p, q);
		q = temp;
	}

	return NULL;
}

/*
 * Reverses the list.
 */
void list_reverse()
{
	node *aux = head;
	head = tail;
	tail = aux;
}

/*
 * This function traverses the list forwards and prints it.
 */
void list_forward_traverse()
{
	node *p = head;
	node *q = nil;
	node *temp;

	while (p != nil)
	{
		printf("%d ", p->data);
		temp = p;
		p = succ(p, q);
		q = temp;
	}
}

/*
 * This function traverses the list backwards and prints it.
 */
void list_backward_traverse()
{
	node *p = tail;
	node *q = nil;
	node *temp;

	while (p != nil)
	{
		printf("%d ", p->data);
		temp = p;
		p = pred(p, q);
		q = temp;
	}
}

/*
 * Clears the screen and prints a list of commands.
 */
void print_available_commands()
{
	clrscr();
	printf("\n");
	printf("Available commands:\n");
	printf(" i - insert a new node at the beginning of the list.\n");
	printf(" d - delete a node from the list.\n");
	printf(" l - search for a node in the list.\n");
	printf(" s - find a node's successor.\n");
	printf(" p - find a node's predecessor.\n");
	printf(" f - list the nodes in the list going forwards.\n");
	printf(" b - list the nodes in the list going backwards.\n");
	printf(" r - reverse the list.\n");
	printf(" ESC - end program.\n\n");
	printf("Your command: ");
}

void main()
{
	char c;
	int n;
	node *x;

	do
	{
		print_available_commands();
		c = getch();
		if (c == 0)
			c = getch();
		printf("%c", c);
		switch (c)
		{
			case 'i':
				printf("\nData to insert: ");
				x = new node;
				scanf("%d", &(x->data));
				list_insert(x);
				printf("Node inserted. Press any key to continue.");
				getch();
				break;
			case 'd':
				printf("\nData to delete: ");
				scanf("%d", &n);
				x = list_search(n);
				if (x != NULL)
				{
					list_delete(x);
					printf("Node deleted. Press any key to continue.");
				}
				else
					printf("Not found in list. Press any key to continue.");
				getch();
				break;
			case 'l':
				printf("\nData to search: ");
				scanf("%d", &n);
				if (list_search(n) != NULL)
					printf("Found in list. Press any key to continue.");
				else
					printf("Not found in list. Press any key to continue.");
				getch();
				break;
			case 's':
				printf("\nFind successor of: ");
				scanf("%d", &n);
				x = list_succ(n);
				if (x == NULL)
					printf("Element not found in list.");
				else
					if (x == nil)
						printf("Element is last in the list.");
					else
						printf("Succesor of %d is %d.", n, x->data);
				printf(" Press any key to continue.");
				getch();
				break;
			case 'p':
				printf("\nFind predecessor of: ");
				scanf("%d", &n);
				x = list_pred(n);
				if (x == NULL)
					printf("Element not found in list.");
				else
					if (x == nil)
						printf("Element is first in the list.");
					else
						printf("Predecesor of %d is %d.", n, x->data);
				printf(" Press any key to continue.");
				getch();
				break;
			case 'f':
				printf("\nList forwards: ");
				list_forward_traverse();
				printf("\nPress any key to continue.");
				getch();
				break;
			case 'b':
				printf("\nList backwards: ");
				list_backward_traverse();
				printf("\nPress any key to continue.");
				getch();
				break;
			case 'r':
				list_reverse();
				printf("\nList has been reversed.");
				printf(" Press any key to continue.");
				getch();
				break;
			case 27:
				break;
			default:
				printf("\nUnknown command. Press any key to continue.");
				getch();
				break;
		}
	}
	while (c != 27);
}