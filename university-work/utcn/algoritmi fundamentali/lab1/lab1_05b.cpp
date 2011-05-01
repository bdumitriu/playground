/*
 * Aplicara algoritmul de sortare prin interclasare (mergesort) pe
 * datele din fisierul "datetest.dat".
 *
 * Autor: Bogdan DUMITRIU
 * Grupa: 3231
 * Data:  28.10.2001
 */

#include <stdlib.h>
#include <stdio.h>
#include <conio.h>
#include <math.h>

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
		merge_sort(a, 0, n-1);
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