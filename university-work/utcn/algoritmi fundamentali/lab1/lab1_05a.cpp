/*
 * Algoritmul de sortare prin interclasare (mergesort).
 *
 * Autor: Bogdan DUMITRIU
 * Grupa: 3231
 * Data:  28.10.2001
 */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "dataops.h"

int comp = 0;
int atr = 0;

void merge(int *a, int li, int ls, int med)
{
	int i, j, k;
	int *c;

	if ((c = (int*) malloc((ls-li+1)*sizeof(int))) == NULL)
	{
		printf("Memorie insuficienta!");
		getch();
		exit(1);
	}

	i = li;
	j = med+1;
	k = 0;
	while ((i < med+1) && (j < ls+1))
	{
												comp++;
		if (a[i] < a[j])
		{
			c[k] = a[i];						atr++;
			i++;
		}
		else
		{
			c[k] = a[j];						atr++;
			j++;
		}
		k++;
	}
	if (i == med+1)
	{
		for (i = j; i < ls+1; i++)
		{
			c[k] = a[i];						atr++;
			k++;
		}
	}
	else
	{
		for (j = i; j < med+1; j++)
		{
			c[k] = a[j];						atr++;
			k++;
		}
	}
	for (i = li; i <= ls; i++)
	{
		a[i] = c[i-li];							atr++;
	}

    free(c);
}

void merge_sort(int *a, int li, int ls)
{
	int med;

	if (li < ls)
	{
		med = floor((ls-li)/2)+li;
		merge_sort(a, li, med);
		merge_sort(a, med+1, ls);
		merge(a, li, ls, med);
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
	merge_sort(a, 0, n-1);

	printf("\ncomparatii: %d\natribuiri: %d\n\nSirul ordonat: ", comp, atr);

	// se tipareste sirul sortat
	print_array(a, n, 1);
}