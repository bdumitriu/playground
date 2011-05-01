/*
 * Aplicarea algoritmului de sortare prin insertie binara pe datele
 * din fisierul "datetest.dat".
 *
 * Autor: Bogdan DUMITRIU
 * Grupa: 3231
 * Data:  27.10.2001
 */

#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <conio.h>

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
		binary_insertion_sort(a, n);
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