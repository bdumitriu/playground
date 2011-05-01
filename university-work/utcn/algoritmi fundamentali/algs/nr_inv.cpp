/*
 * Algoritmul de aflare a numarului de inversiuni dintr-un sir dat cu eficienta
 * O(nlgn).
 *
 * Numim inversiune a sirului A o pereche de forma (A[i],A[j]) cu i < j
 * si A[i] > A[j].
 *
 * Autor: Bogdan DUMITRIU
 * Data:  03.10.2001
 */

#include <stdio.h>
#include <conio.h>

int inv = 0;

void interclasare(int *a, int p, int q, int r)
{
	int i = p;
	int j = q+1;
	int k = 0;
	int b[100];

	while ((i < q+1) && (j < r+1))
	{
		if (a[i] > a[j])
		{
			inv += q+1-i;
			b[k] = a[j];
			j++;
		}
		else
		{
			b[k] = a[i];
			i++;
		}
		k++;
	}

	if (i == q+1)
	{
		for (i = j; i <= r; i++)
			b[k++] = a[i];
	}
	else
	{
		for (j = i; j <= q; j++)
			b[k++] = a[j];
	}

	for (i = 0; i < k; i++)
		a[i+p] = b[i];
}

void inversiuni(int *a, int li, int ls)
{
	if (li < ls)
	{
		int q = (int) ((ls-li)/2+li);
		inversiuni(a, li, q);
		inversiuni(a, q+1, ls);
		interclasare(a, li, q, ls);
    }
}

void main()
{
    int i, n;
    int a[100];
    
    printf("Numarul de elemente: ");
    scanf("%d", &n);

    for (i = 0; i < n; i++)
    {
		printf(" elem. %d: ", i+1);
		scanf("%d", &a[i]);
	}

	inversiuni(a, 0, n-1);

	printf("Sirul sortat este: ");

	for (i = 0; i < n; i++)
	{
		printf("%d ", a[i]);
	}
	printf("\nNumarul de inversiuni din sirul initial este %d.\n", inv);

	getch();
}