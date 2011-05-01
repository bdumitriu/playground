/*
 * This program sorts the vertices of an oriented graph in
 * topological order. That means that, given an oriented graph,
 * this algorithm will return a list of all its nodes with the
 * propery that if node a is before node b in this list than
 * edge (b,a) doesn't exist.
 *
 * Author: Bogdan DUMITIRU
 * Date:   07.12.2001
 */

#include <stdio.h>
#include <conio.h>

struct list_node
{
	struct list_node *next;
	int data;
};

struct vertex
{
	struct list_node *head;
	int node;
};

/*
 * Adds a new node to the list that starts with
 * head. It should be called this way:
 *	list_add(&current_head, int_value).
 * where current_head should be a list_node*.
 */
void list_add(struct list_node **head, int data)
{
	struct list_node *ln = new list_node;
	ln->data = data;
	ln->next = *head;
	*head = ln;
}

/*
 * Returns the index of the first of the first n elements of
 * array succ for which pred[index] = 0. If no such element
 * is found -1 is returned.
 */
int find_next(int pred[100], int n)
{
	for (int i = 1; i <= n; i++)
		if (pred[i] == 0)
			return i;
	return -1;
}

void main()
{
	vertex vertices[100];
	vertex vx;
	int pred[100];
	int list[100];
	int n, m, v, v1, v2;
	int idx = 0;

	clrscr();
	printf("Number of vertices in graph: ");
	scanf("%d", &n);

	vx.head = NULL;
	for (int i = 1; i <= n; i++)
	{
		vx.node = i;
		vertices[i] = vx;
		pred[i] = 0;
	}

	printf("Number of edges in graph: ");
	scanf("%d", &m);

	for (i = 0; i < m; i++)
	{
		printf(" edge %d between nodes: ", i+1);
		scanf("%d %d", &v1, &v2);
		list_add(&(vertices[v1].head), v2);
		pred[v2]++;
	}

	while ((v = find_next(pred, n)) != -1)
	{
		list[idx++] = v;
		pred[v] = -1;
		list_node *p = vertices[v].head;
		while (p != NULL)
		{
			if (pred[p->data] != 0)
				pred[p->data]--;
			p = p->next;
		}
	}

	if (idx != n)
		printf("The graph cannot be topologically sorted.");
	else
	{
		printf("A topological sort of the graph is:\n\t");
		for (i = 0; i < idx; i++)
			printf("%d ", list[i]);
	}

	getch();
}