/*
 * Data input/output operations library.
 *
 * Author:  Bogdan DUMITRIU
 * Version: 1.0
 * Date:    27.10.2001
 */

#include "dataops.h"

int read_array(int *array)
{
	int n;

	printf("Numarul de elemente: ");
	if (scanf("%d", &n) == 0)
		return -1;

	for (int i = 0; i < n; i++)
	{
		printf("elementul %d: ", i+1);
		if (scanf("%d", &array[i]) == 0)
			return -1;
	}

	return n;
}

int read_pascal_array(int *array)
{
	int n;

	printf("Numarul de elemente: ");
	if (scanf("%d", &n) == 0)
		return -1;

	for (int i = 1; i <= n; i++)
	{
		printf("elementul %d: ", i);
		if (scanf("%d", &array[i]) == 0)
			return -1;
	}

	return n;
}

int print_array(int *array, int array_length, int stop_for_read)
{
	for (int i = 0; i < array_length-1; i++)
	{
		if (printf("%d ", array[i]) == EOF)
			return -1;
	}
	if (array_length > 0)
		if (printf("%d", array[i]) == EOF)
			return -1;

	if (stop_for_read)
	{
		getch();
	}

	return 0;
}

int print_pascal_array(int *array, int array_length, int stop_for_read)
{
	for (int i = 1; i < array_length; i++)
	{
		if (printf("%d ", array[i]) == EOF)
			return -1;
	}
	if (array_length > 0)
		if (printf("%d", array[i]) == EOF)
			return -1;

	if (stop_for_read)
	{
		getch();
	}

	return 0;
}