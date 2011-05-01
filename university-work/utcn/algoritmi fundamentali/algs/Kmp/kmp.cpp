/*
 * An implementation of the Knuth-Morris-Pratt algorithm (KMP).
 *
 * Author: Bogdan DUMITRIU
 * Date:   03.02.2002
 */

#include <stdio.h>
#include <conio.h>
#include <string.h>
#include <stdlib.h>

int* prefix_function(char *model)
{
	int m = strlen(model);
	int *pi;
	int k = -1;

	if ((pi = (int *) malloc(m*sizeof(int))) == NULL)
	{
		printf("Not enough memory.");
		getch();
		exit(EXIT_FAILURE);
	}

	pi[0] = -1;
	for (int i = 1; i < m; i++)
	{
		while ((k > -1) && (model[k+1] != model[i]))
			k = pi[k];
		if (model[k+1] == model[i])
			k++;
		pi[i] = k;
	}

	return pi;
}

void kmp(char *string, char *substring)
{
	int n = strlen(string);
	int m = strlen(substring);
	int q = -1;
	int *pi;

	pi = prefix_function(substring);
	for (int i = 0; i < n; i++)
	{
		while ((q > -1) && (substring[q+1] != string[i]))
			q = pi[q];
		if (substring[q+1] == string[i])
			q++;
		if (q == m-1)
		{
			printf("The substring appears at position %i.\n", i-m+1);
			q = pi[q];
		}
	}
}

void main()
{
	int *x;
	char *sub;
	char *str;

	clrscr();
	strcpy(sub, "live");
	strcpy(str, "I will live forever, or die trying.");
	kmp(str, sub);
	getch();
}