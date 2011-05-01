/*
 * Algoritmul de cautare binara a unui element intr-un sir.
 *
 * Autor: Bogdan DUMITRIU
 * Data:  21.10.2001
 */

#include <stdio.h>
#include <conio.h>
#include <alloc.h>

void main()
{
	int n, elem;
	int *a;

	clrscr();

	// Citirea datelor de intrare.
	printf("Numarul de elemente: ");
	scanf("%d", &n);

	if ((a = (int*) malloc((n+1)*sizeof(int))) == NULL)
	{
		printf("Not enough memory.");
		getch();
		exit(0);
	}

	for (int i = 1; i <= n; i++)
	{
		printf(" elementul %d: ", i);
		scanf("%d", &a[i]);
	}

	printf("Elementul cautat: ");
	scanf("%d", &elem);

	printf("Sirul de numere introdus: ");
	for (i = 1; i <= n; i++)
	{
		printf("%d ", a[i]);
	}
	printf("\nElementul cautat: %d.\n", elem);

	// De aici incepe algoritmul de cautare binara efectiv.
	int poz;
	int li = 1;
	int ls = n;
	int gata = 0;
	int rez = 0;

	while ((!gata) && (li <= ls))
	{
		poz = ((ls-li)/2)+li;
		if (a[poz] == elem)
		{
			rez = 1;
			gata = 1;
		}
		else
		{
			if (a[poz] < elem)
			{
				li = poz+1;
			}
			else
			{
				ls = poz-1;
			}
		}
	}

	if (rez == 1)
	{
		printf("Gasit.");
	}
	else
	{
		printf("Negasit.");
	}

	free(a);
	getch();
}