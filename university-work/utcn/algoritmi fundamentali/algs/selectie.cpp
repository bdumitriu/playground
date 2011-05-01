/*
 * Author: Bogdan DUMITRIU
 * Date:   06.11.2001
 *
 * This algorithm determines the element of an unsorted array that would be
 * on the i-th position in the array with its elements sorted in ascending
 * order with an efficiency of O(n).
 */

#include <stdio.h>
#include <stdlib.h>
#include <alloc.h>
#include <conio.h>
#include <math.h>

/*
 * Sorts the array a by direct insertion and returns the superior
 * mean value of the array. I_lim and s_lim are the inferior and
 * superior limits of the array respectively. Only the area bewteen
 * i_lim and s_lim will be sorted!
 */
int mean_value(int *a, int i_lim, int s_lim)
{
	int i, j, aux, val;
	
	for (i = i_lim+1; i <= s_lim; i++)
	{
		j = i;
		aux = a[i];
		while ((a[j-1] > aux) && (j > i_lim))
		{
			a[j] = a[j-1];
			j--;
		}
		a[j] = aux;
	}
	
	val = (int) (s_lim-i_lim)/2;
	
	if ((s_lim-i_lim+1)%2 == 0)
		return a[i_lim+val+1];
	else
		return a[i_lim+val];
}

/*
 * Partitions the portion between i_lim and s_lim of array a
 * in the elements lower than key and the elements greater
 * than key.
 */
int partition(int *a, int key, int i_lim, int s_lim)
{
	int *temp;
	int idx1 = 0;
	int idx2 = s_lim-i_lim;

	if ((temp = (int*) malloc((s_lim+1-i_lim)*sizeof(int))) == NULL)
	{
		printf("Not enough memory.\n");
		getch();
		exit(1);
	}

	for (int i = i_lim; i <= s_lim; i++)
	{
		if (a[i] < key)
			temp[idx1++] = a[i];
		if (a[i] > key)
			temp[idx2--] = a[i];
	}

	for (i = idx1; i <= idx2; i++)
		temp[i] = key;

	for (i = i_lim; i <= s_lim; i++)
		a[i] = temp[i-i_lim];

	return i_lim+idx1;
}

/*
 * The actul selection algorithm.
 */
int selection(int *a, int pos, int i_lim, int s_lim)
{
	int *b;
	int size = (int) (s_lim-i_lim+1)/5;
	int i, mean, mean_pos;

	if ((s_lim-i_lim+1)%5 != 0)
		size++;

	if ((b = (int*) malloc(size*sizeof(int))) == NULL)
	{
		printf("Not enough memory.\n");
		getch();
		exit(1);
	}

	for (i = 0; i < size-1; i++)
	{
		b[i] = mean_value(a, i_lim+i*5, i_lim+i*5+4);
	}
	b[size-1] = mean_value(a, i_lim+(size-1)*5, s_lim);

	if (size <= 5)
		mean = mean_value(b, 0, size-1);
	else
		mean = selection(b, (size-1)/2, 0, size-1);

	mean_pos = partition(a, mean, i_lim, s_lim);

	if ((i_lim+pos-1) < mean_pos)
		return selection(a, pos, i_lim, mean_pos-1);
	else
		if ((i_lim+pos-1) > mean_pos)
			return selection(a, pos-(mean_pos-i_lim+1), mean_pos+1, s_lim);
		else
			return mean;
}

/*
 * A front-end to the selection algorithm. Returns the element
 * that would be on position pos in the sorted version of the
 * a array. Array a is modified in the process.
 */
int selection(int *a, int nr_of_elements, int pos)
{
	return selection(a, pos, 0, nr_of_elements-1);
}

void main()
{
	int *a;
	int i, n, pos;
	int test = 0;

	// read data from user
	printf("Number of elements: ");
	scanf("%d", &n);

	if ((a = (int*) malloc(n*sizeof(int))) == NULL)
	{
		printf("Not enough memory.\n");
		exit(1);
	}

	for (i = 0; i < n; i++)
	{
		printf(" element %d: ", i+1);
		scanf("%d", &a[i]);
	}

	do
	{
		if (test)
			printf("Position must be between 1 and %d.\n", n);
		test = 1;
		printf("Position: ");
		scanf("%d", &i);
	}
	while (!((i >= 1) && (i <= n)));

	// call the selection algorithm
	pos = selection(a, n, i);

	// print the result
	printf("The element that would on position %d in the sorted array", i);
	printf(" is %d.\n", pos);
	getch();
}
