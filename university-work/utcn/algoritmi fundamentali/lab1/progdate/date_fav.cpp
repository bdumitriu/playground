/*
 * This program generates favorable (ordered set) test data for
 * sorting algorithms.
 * Usage:
 *		<program_name> <output_file>
 *
 * Author:  Bogdan DUMITRIU
 * Version: 1.0
 * Date:    28.10.2001
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <conio.h>
#include <math.h>

void genereaza(int size, int *vector)
{
	for (int i = 0; i < size; i++)
	{
		vector[i] = i;
	}
}

void main(int argc, char *argv[])
{
	FILE *f;
	int *vector;

	if (argc != 2)
	{
		char *ptr, c = '\\';
		char *program_name;
		int pos;

		// Since in argv[0] we have something like
		// 		C:\\PATH_TO_PROGRAM\\PROGRAM_NAME.EXE
		// we have to strip it down to program_name.exe in
		// order to print a nice usage message to the user.
		// It is for that that we need all this yada-yada that
		// follows.
		if ((program_name = (char*) malloc(30*sizeof(char))) == NULL)
		{
			printf("Not enough memory.");
			getch();
			exit(1);
		}
		ptr = strrchr(argv[0], c);
		pos = ptr-argv[0]+1;
		for (int i = pos; i < strlen(argv[0]); i++)
		{
			program_name[i-pos] = argv[0][i];
		}
		program_name[i-pos] = '\0';
		program_name = (char*) strlwr(program_name);

		// Now we can finally print the nice usage message to the
		// user and exit.
		printf("\nUsage:\n\t%s <output_file>", program_name);
		getch();
		exit(1);
	}

	if ((vector = (int *) malloc(200*sizeof(int))) == NULL)
	{
		printf("Not enough memory.");
		getch();
		exit(1);
	}

	if ((f = fopen(argv[1], "w")) == NULL)
	{
		printf("Couldn't create output file. Check for enough");
		printf(" space & rights.");
		getch();
		exit(1);
	}

	for (int i = 1; i <= 20; i++)
	{
		genereaza(i*10, vector);

		fprintf(f, "%d\n", i*10);
		for (int j = 0; j < i*10-1; j++)
		{
			fprintf(f, "%d ", vector[j]);
		}
		fprintf(f, "%d", vector[i*10-1]);
		if (i != 20)
		{
			fprintf(f, "\n");
		}
	}

	fclose(f);
}
