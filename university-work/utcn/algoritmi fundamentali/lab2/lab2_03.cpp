/*
 * Author: Bogdan DUMITRIU
 * Date:   23.11.2001
 *
 * This program reads a number of sorted sequences from the user
 * and merges them into one big sorted sequence with an O(nlg(k))
 * efficiency, where k is the number of sequences and n is the
 * total number of elements in all sequences. To obtain this
 * efficiency a heap of k elements is used.
 */

#include <stdio.h>
#include <conio.h>
#include <stdlib.h>

typedef struct heap_element
{
	int idx;	// the sequence the element has been taken from
	int value;	// the actual value of the element
} he;

/*
 * The heap structure used.
 */
he heap[21];

/*
 * The dimension of the heap.
 */
int dim = 0;

/*
 * The initial sequences.
 */
int seq[20][50];

/*
 * The final sorted sequence.
 */
int fseq[1000];

/*
 * The lenght of each sequence.
 */
int length[20];

/*
 * The number of sequences and the total number of elements
 * respectively.
 */
int k, n;

int parent(int idx)
{
	return (int) (idx/2);
}

int left(int idx)
{
	return 2*idx;
}

int right(int idx)
{
	return 2*idx+1;
}

/*
 * Assuming that the left and right son of idx are heaps,
 * this function makes idx a heap too.
 */
void rebuild_heap(int idx)
{
	int l = left(idx);
	int r = right(idx);

	if (l > dim)
		return;

	he aux;
	int min_pos = l;
	if ((r <= dim) && (heap[r].value < heap[min_pos].value))
		min_pos = r;

	if (heap[min_pos].value >= heap[idx].value)
		return;
	else
	{
		aux = heap[min_pos];
		heap[min_pos] = heap[idx];
		heap[idx] = aux;
		rebuild_heap(min_pos);
	}
}

/*
 * Returns the minimum value from the heap and rebuilds it
 * without this minimum value, also decreasing its dimension
 * by 1.
 */
he get_min()
{
	he aux;

	aux = heap[1];
	heap[1] = heap[dim];
	dim--;
	rebuild_heap(1);

	return aux;
}

/*
 * Inserts element into the heap, making sure that the heap
 * remains a heap.
 */
void insert_into_heap(he element)
{
	dim++;
	int pos = dim;
	while ((pos > 1) && (heap[parent(pos)].value > element.value))
	{
		heap[pos] = heap[parent(pos)];
		pos = parent(pos);
	}
	heap[pos] = element;
}

void create_sorted_sequence()
{
	he el;
	int pos[20], idx = 0;

	for (int i = 0; i < k; i++)
	{
		if (length[i] >= 1)
		{
			el.value = seq[i][0];
			el.idx = i;
			insert_into_heap(el);
			pos[i] = 1;
		}
		else
			pos[i] = 0;
	}
	while (dim > 0)
	{
		el = get_min();		// take the element out of the heap...
		fseq[idx++] = el.value;	// ...and put it into the sorted sequence

		// see which sequence the next element to add into the
		// heap will be from.
		int s = 0;

		// if we still have elements in the sequence from which
		// the last extracted minimum has been from than this is the one
		if (pos[el.idx] < length[el.idx])
			s = el.idx;

		// otherwise, we look for the first sequence that still
		// has elements...
		else
		{
			int gata = 0;
			while ((s < k) && (!gata))
			{
				if (pos[s] < length[s])
					gata = 1;
				else
					s++;
			}
		}
		if (s < k)
		{
			el.value = seq[s][pos[s]];
			el.idx = s;
			pos[s]++;
			insert_into_heap(el);
		}
	}
}

void main()
{
	int idx = 0;

	clrscr();
	printf("The total number of elements: ");
	scanf("%d", &n);

	printf("Now please type in the elements of the sorted sequences.\n");
	printf("The maximum number of sequences allowed is 20.\n");
	printf("The maximum number of numbers allowed in a sequence is 50.\n");
	printf("\n");

	int j = 0;
	printf(" Type the elements of sequence 1");
	printf(" (type 0 to end the current sequence):\n");
	for (int i = 0; i-idx < n; i++)
	{
		printf("  element: ");

		if ((j < 50) && (idx < 20))
			scanf("%d", &seq[idx][j]);
		else
			if (idx >= 20)
			{
				printf("\n");
				printf("Maximum number of sequences exceeded.");
				printf("\nExiting...");
				getch();
				exit(0);
			}
			else
			{
				printf("\n");
				printf("Maximum number of elements in a sequence exceeded.");
				printf("\nExiting...");
				getch();
				exit(0);
			}
		if (seq[idx][j] == 0)
		{
			length[idx] = j;
			j = 0;
			idx++;
			printf(" Sequence %d finished (%d elements stored).\n", idx,
				length[idx-1]);
			printf("\n Type the elements of sequence %d", idx+1);
			printf(" (type 0 to end the current sequence):\n");
		}
		else
			j++;
	}
	length[idx] = j;
	idx++;
	printf(" Sequence %d finished (%d elements stored).\n", idx,
		length[idx-1]);
	k = idx;

	printf("\nThe sequences are:\n");
	i = 0;
	while (i < k)
	{
		j = 0;
		printf(" Sequence %d:", i+1);
		while (j < length[i])
		{
			printf(" %d", seq[i][j]);
			j++;
		}
		printf("\n");
		i++;
	}

	create_sorted_sequence();
	printf("\nThe sorted sequence is:");
	for (i = 0; i < n; i++)
		printf(" %d", fseq[i]);
	getch();
}