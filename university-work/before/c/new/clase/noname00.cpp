#include <stdio.h>

void main()
{
	char **buf = (char**) new char [500*500];

	for (int i = 0; i < 500; i++)
		for (int j = 0; j < 500; j++)
			buf[i][j] = ' ';
}