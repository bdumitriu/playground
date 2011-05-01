/*
 * Algoritmul de sortare rapida (quicksort).
 *
 * Autor: Bogdan DUMITRIU
 * Grupa: 3231
 * Data:  28.10.2001
 */

#include <stdlib.h>
#include <math.h>
#include "dataops.h"

int comp = 0;
int atr = 0;

int partitionare(int *a, int li, int ls)
{
	int i, j, pivot, aux;

	i = li-1;
	j = ls+1;
	pivot = a[li];							atr++;
	while (1)
	{
		do
		{
			j--;							comp++;
		}
		while (a[j] > pivot);
		do
		{
			i++;							comp++;
		}
		while (a[i] < pivot);

		if (i < j)
		{
			aux = a[i];						atr++;
			a[i] = a[j];					atr++;
			a[j] = aux;						atr++;
		}
		else
		{
			return j;
		}
	}
}

void quick_sort(int *a, int li, int ls)
{
	int poz;

	if (ls > li)
	{
		poz = partitionare(a, li, ls);
		quick_sort(a, li, poz);
		quick_sort(a, poz+1, ls);
	}
}

void main()
{
	int a[201], n;

	clrscr();

	// se citeste sirul
	n = read_array(a);

	// daca a aparut o eroare la citirea datelor se iese din program
	if (n == -1)
	{
		printf("Eroare la citirea datelor!");
		getch();
		exit(1);
	}

	// se apeleaza algoritmul de sortare
	quick_sort(a, 0, n-1);

	printf("\ncomparatii: %d\natribuiri: %d\n\nSirul ordonat: ", comp, atr);

	// se tipareste sirul sortat
	print_array(a, n, 1);
}