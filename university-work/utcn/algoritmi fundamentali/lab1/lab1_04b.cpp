/*
 * Aplicarea algoritmului de sortare rapida (quicksort) pe datele
 * din fisierul "datetest.dat".
 *
 * Autor: Bogdan DUMITRIU
 * Grupa: 3231
 * Data:  28.10.2001
 */

#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <conio.h>

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

void main(int argc, char *argv[])
{
	int a[201], n;
	FILE *f, *g;

	if (argc != 3)
	{
		printf("\nUsage:\n\t<program_name> <input_file> <output_file>");
		getch();
		exit(1);
	}

	if ((f = fopen(argv[1], "r")) == NULL)
	{
		printf("Nu am gasit fisierul %s in directorul", argv[1]);
		printf(" curent.\n");
		getch();
		exit(1);
	}

	if ((g = fopen(argv[2], "w")) == NULL)
	{
		printf("Nu am putut crea fisierul %s in ", argv[2]);
		printf("directorul curent.\n");
		getch();
		exit(1);
	}

	while (!feof(f))
	{
		fscanf(f, "%d", &n);
		for (int i = 0; i < n; i++)
		{
			fscanf(f, "%d", &a[i]);
		}
		comp = atr = 0;
		quick_sort(a, 0, n-1);
		fprintf(g, "%d %d %d", n, comp, atr);
		if (!feof(f))
		{
			fprintf(g, "\n");
		}
	}
	fclose(f);
	fclose(g);

	printf("Gata!");
	getch();
}