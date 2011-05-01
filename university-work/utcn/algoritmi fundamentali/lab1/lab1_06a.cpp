/*
 * Algoritmul de sortare cu bule (bubble sort).
 *
 * Autor: Bogdan DUMITRIU
 * Grupa: 3231
 * Data:  28.10.2001
 */

#include <stdlib.h>
#include "dataops.h"

int comp = 0;
int atr = 0;

void bubble_sort(int *a, int n)
{
	int i, j, aux, gata;

	j = n-1;
	do
	{
		gata = 1;
		for (i = 0; i < j; i++)
		{
											comp++;
			if (a[i] > a[i+1])
			{
				aux = a[i];					atr++;
				a[i] = a[i+1];				atr++;
				a[i+1] = aux;				atr++;
				gata = 0;
			}
		}
		j--;
	}
	while (!gata);
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
	bubble_sort(a, n);

	printf("\ncomparatii: %d\natribuiri: %d\n\nSirul ordonat: ", comp, atr);

	// se tipareste sirul sortat
	print_array(a, n, 1);
}