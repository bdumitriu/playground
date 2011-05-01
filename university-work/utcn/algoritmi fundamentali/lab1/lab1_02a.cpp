/*
 * Algoritmul de sortare prin insertie binara.
 *
 * Autor: Bogdan DUMITRIU
 * Grupa: 3231
 * Data:  27.10.2001
 */

#include <stdlib.h>
#include <math.h>
#include "dataops.h"

int comp = 0;
int atr = 0;

void binary_insertion_sort(int *a, int n)
{
	int aux, li, ls, med, i, j, idx, gata;

	for (i = 1; i < n; i++)
	{
		aux = a[i];							atr++;
		li = 0;
		ls = i-1;
		gata = 0;
		do
		{
			med = floor((ls-li)/2)+li;		comp++;
			if (a[med] < aux)
			{
				li = med+1;
			}
			else
			{
											comp++;
				if (a[med] > aux)
				{
					ls = med-1;
				}
				else
				{
					gata = 1;
				}
			}
		}
		while ((!gata) && (li <= ls));

		if (gata)
		{
			idx = med;
		}
		else
		{
			idx = li;
		}

		for (j = i; j > idx; j--)
		{
			a[j] = a[j-1];					atr++;
		}
		a[idx] = aux;						atr++;
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
	binary_insertion_sort(a, n);

	printf("\ncomparatii: %d\natribuiri: %d\n\nSirul ordonat: ", comp, atr);

	// se tipareste sirul sortat
	print_array(a, n, 1);
}