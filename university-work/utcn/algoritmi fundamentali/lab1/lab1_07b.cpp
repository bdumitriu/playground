/*
 * Aplicarea algoritmului de sortare prin cu ajutorul unui heap
 * (heapsort) pe datele din fisierul "datetest.dat".
 *
 * Autor: Bogdan DUMITRIU
 * Grupa: 3231
 * Data:  10.11.2001
 */

#include <stdlib.h>
#include <stdio.h>
#include <conio.h>

int comp, atr;

int parinte(int i)
{
	return (int) (i/2);
}

int stanga(int i)
{
	return 2*i;
}

int dreapta(int i)
{
	return 2*i+1;
}

void reconstituie_heap(int *a, int dim, int i)
{
	int idx_max, aux;
	int s = stanga(i);
	int d = dreapta(i);

	if (s > dim)
		return;

	idx_max = s;
	if (d <= dim)
	{
														comp++;
		if (a[d] > a[idx_max])
			idx_max = d;
	}

														comp++;
	if (a[i] >= a[idx_max])
		return;
	else
	{
		aux = a[idx_max];								atr++;
		a[idx_max] = a[i];								atr++;
		a[i] = aux;										atr++;
		reconstituie_heap(a, dim, idx_max);
	}
}

void construieste_heap(int *a, int dim)
{
	for (int i = (int) (dim/2); i > 0; i--)
		reconstituie_heap(a, dim, i);
}

void heap_sort(int *a, int dim)
{
	int aux;

	construieste_heap(a, dim);
	while (dim > 1)
	{
		aux = a[dim];									atr++;
		a[dim] = a[1];									atr++;
		a[1] =  aux;									atr++;
		dim--;
		reconstituie_heap(a, dim, 1);
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
		heap_sort(a, n);
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