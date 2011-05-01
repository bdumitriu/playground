#include <stdio.h>
#include <iostream.h>

void main()
{
	char **buf = (char**) new char [500*255];

	for (i = 0; i < 500; i++)
		for (int j = 0; j < 255; j++)
			buf[i][j] = ' ';

	for (i = 0; i < 500; i++)
		for (int j = 0; j < 255; j++)
			cout<<buf[i][j];
}