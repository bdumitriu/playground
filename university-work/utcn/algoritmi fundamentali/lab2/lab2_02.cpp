/*
 * Author: Bogdan DUMITRIU
 * Date:   11.11.2001
 *
 * This algorithm solves the following problem:
 *	Let X[1..n] and Y[1..n] be two sorted arrays that contain
 *	n elements each. Create an algorithm of complexity O(lgn)
 *	that determines the mean value of all 2n elements.
 */

#include <stdio.h>
#include <conio.h>

/*
 * This procedure returns the inferior mean value of the 2 arrays.
 */
int select_mean_value(int *a, int *b, int il_a, int sl_a, int il_b, int sl_b)
{
	if ((il_a == sl_a) && (il_b == sl_b))
		if (a[il_a] < b[il_b])
			return a[il_a];
		else
			return b[il_b];
	else
	{
		int ma = (int) (il_a+sl_a)/2;
		int mb = (int) (il_b+sl_b)/2;

		if (a[ma] == b[mb])
			return a[ma];

		if (a[ma] > b[mb])
		{
			if (((sl_b-il_b)%2) == 0)
				mb--;
			return select_mean_value(a, b, il_a, ma, mb+1, sl_b);
		}
		else
		{
			if (((sl_a-il_a)%2) == 0)
				ma--;
			return select_mean_value(a, b, ma+1, sl_a, il_b, mb);
		}
	}
}

void main()
{
	int a[20];
	int b[20];
	int n;

	clrscr();

	printf("Number of elements in each array: ");
	scanf("%d", &n);

	printf(" The elements of the first array ");
	printf("(have to be in ascending order):\n");
	for (int i = 0; i < n; i++)
	{
		printf("  element %d: ", i+1);
		scanf("%d", &a[i]);
	}

	printf(" The elements of the second array ");
	printf("(have to be in ascending order):\n");
	for (i = 0; i < n; i++)
	{
		printf("  element %d: ", i+1);
		scanf("%d", &b[i]);
	}

	int m = select_mean_value(a, b, 0, n-1, 0, n-1);
	printf("The mean value of the two arrays is %d.\n", m);
	getch();
}