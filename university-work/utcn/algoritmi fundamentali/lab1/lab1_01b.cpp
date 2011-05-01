/*
 * Aplicarea algoritmului de sortare prin insertie directa pe datele
 * din fisierul "datetest.dat".
 *
 * Autor: Bogdan DUMITRIU
 * Grupa: 3231
 * Data:  27.10.2001
 */

#include <stdlib.h>
#include <stdio.h>
#include <conio.h>

int comp, atr;

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
		direct_insertion_sort(a, n);
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