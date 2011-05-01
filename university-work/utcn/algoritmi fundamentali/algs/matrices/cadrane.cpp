/*
 * This program computes the arithmetical averages of each of the 4
 * quadrants of a matrix. The quadrants are delimited by the matrix's
 * diagonals. The elements on the diagonals will not be included.
 *
 * Author: Bogdan DUMITRIU
 * Date:  07.12.2001
 */

#include <stdio.h>
#include <conio.h>

void main()
{
	int n, matrix[20][20];
	int north_quadrant = 0;
	int south_quadrant = 0;
	int east_quadrant = 0;
	int west_quadrant = 0;

	clrscr();
	printf("The number of lines/columns of the matrix: ");
	scanf("%d", &n);
	printf("Type the matrix's elements:\n");
	for (int i = 0; i < n; i++)
		for (int j = 0; j < n; j++)
		{
			printf(" element %d,%d: ", i+1, j+1);
			scanf("%d", &matrix[i][j]);
		}

	printf("Your matrix is:");
	for (i = 0; i < n; i++)
	{
		printf("\n\t");
		for (int j = 0; j < n; j++)
			printf("%d ", matrix[i][j]);
	}

	for (i = 0; i < n; i++)
		for (int j = 0; j < n; j++)
		{
			if ((i+j != n-1) && (i-j != 0))	// ca sa nu fie pe diagonale
			{
				if (i+j < n-1)
					if (i-j < 0)
						north_quadrant = north_quadrant+matrix[i][j];
					else
						east_quadrant = east_quadrant+matrix[i][j];
				else
					if (i-j < 0)
						west_quadrant = west_quadrant+matrix[i][j];
					else
						south_quadrant = south_quadrant+matrix[i][j];
			}
		}

	// The number of elements in a quadrant is:
	//      [(n-1)/2]
	//       -------
	//        \
	//         \
	//         /      (n-2*i)
	//        /
	//       -------
	//        i = 1
	int nr = 0;
	for (i = 1; i <= (n-1)/2; i++)
		nr = nr+(n-2*i);

	printf("\n The arithmetical average of the elements");
	printf(" in the north quadrant is %f.", (float) north_quadrant/nr);
	printf("\n The arithmetical average of the elements");
	printf(" in the south quadrant is %f.",	(float) south_quadrant/nr);
	printf("\n The arithmetical average of the elements");
	printf(" in the west quadrant is %f.", (float) west_quadrant/nr);
	printf("\n The arithmetical average of the elements");
	printf(" in the east quadrant is %f.", (float) east_quadrant/nr);
	getch();
}