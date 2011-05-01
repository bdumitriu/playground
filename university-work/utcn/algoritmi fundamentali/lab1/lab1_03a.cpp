/*
 * Algoritmul de sortare prin selectie.
 *
 * Autor: Bogdan DUMITRIU
 * Grupa: 3231
 * Data:  28.10.2001
 */

#include <stdlib.h>
#include "dataops.h"

int comp = 0;
int atr = 0;

void selection_sort(int *a, int n)
{
	int i, j, min, idx;

	for (i = 0; i < n-1; i++)
	{
		idx = i;
		for (j = i+1; j < n; j++)
		{
											comp++;
			if (a[idx] > a[j])
				idx = j;
		}
		min = a[idx];						atr++;
		a[idx] = a[i];						atr++;
		a[i] = min;							atr++;
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
	selection_sort(a, n);

	printf("\ncomparatii: %d\natribuiri: %d\n\nSirul ordonat: ", comp, atr);

	// se tipareste sirul sortat
	print_array(a, n, 1);
}