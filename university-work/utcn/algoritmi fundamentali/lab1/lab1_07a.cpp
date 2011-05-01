/*
 * Algoritmul de sortare folosind un heap (heapsort).
 *
 * Autor: Bogdan DUMITRIU
 * Grupa: 3231
 * Data:  10.11.2001
 */

#include <stdlib.h>
#include <math.h>
#include "dataops.h"

int comp = 0;
int atr = 0;

int parinte(int i)
{
	return (int) (i/2);
}

int stanga(int i)
{
	return 2*i;
}

int dreapta(int i)
{
	return 2*i+1;
}

void reconstituie_heap(int *a, int dim, int i)
{
	int idx_max, aux;
	int s = stanga(i);
	int d = dreapta(i);

	if (s > dim)
		return;

	idx_max = s;
	if (d <= dim)
	{
														comp++;
		if (a[d] > a[idx_max])
			idx_max = d;
	}

														comp++;
	if (a[i] >= a[idx_max])
		return;
	else
	{
		aux = a[idx_max];								atr++;
		a[idx_max] = a[i];								atr++;
		a[i] = aux;										atr++;
		reconstituie_heap(a, dim, idx_max);
	}
}

void construieste_heap(int *a, int dim)
{
	for (int i = (int) (dim/2); i > 0; i--)
		reconstituie_heap(a, dim, i);
}

void heap_sort(int *a, int dim)
{
	int aux;

	construieste_heap(a, dim);
	while (dim > 1)
	{
		aux = a[dim];									atr++;
		a[dim] = a[1];									atr++;
		a[1] =  aux;									atr++;
		dim--;
		reconstituie_heap(a, dim, 1);
	}
}

void main()
{
	int a[202], n;

	clrscr();

	// se citeste sirul
	n = read_pascal_array(a);

	// daca a aparut o eroare la citirea datelor se iese din program
	if (n == -1)
	{
		printf("Eroare la citirea datelor!");
		getch();
		exit(1);
	}

	// se apeleaza algoritmul de sortare
	heap_sort(a, n);

	printf("\ncomparatii: %d\natribuiri: %d\n\nSirul ordonat: ", comp, atr);

	// se tipareste sirul sortat
	print_pascal_array(a, n, 1);
}