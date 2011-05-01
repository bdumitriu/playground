/*
 * Aplicarea algoritmul de sortare cu bule (bubble sort) pe datele
 * din fisierul "datetest.dat".
 *
 * Autor: Bogdan DUMITRIU
 * Grupa: 3231
 * Data:  28.10.2001
 */

#include <stdlib.h>
#include <stdio.h>
#include <conio.h>

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
		bubble_sort(a, n);
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