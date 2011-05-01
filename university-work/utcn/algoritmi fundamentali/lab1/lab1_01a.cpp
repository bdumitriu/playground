/*
 * Algoritmul de sortare prin insertie directa.
 *
 * Autor: Bogdan DUMITRIU
 * Grupa: 3231
 * Data:  27.10.2001
 */

#include <stdlib.h>
#include "dataops.h"

int comp = 0;
int atr = 0;

void direct_insertion_sort(int *a, int n)
{
	int aux, i, j;

	for (i = 1; i < n; i++)
	{
		aux = a[i];							atr++;
		j = i;
											comp++;
		while ((aux < a[j-1]) && (j > 0))
		{
			a[j] = a[j-1];					atr++;
			j = j-1;						comp++;
		}
		a[j] = aux;							atr++;
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
	direct_insertion_sort(a, n);

	printf("\ncomparatii: %d\natribuiri: %d\n\nSirul ordonat: ", comp, atr);

	// se tipareste sirul sortat
	print_array(a, n, 1);
}