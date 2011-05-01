#include <stdio.h>
#include <conio.h>
#include <stdlib.h>
#include <string.h>
/*
void Tija()
{
	int *v = new int [200000], k;
}*/

void main()
{
	long nr;
	char auxil1[20], auxil2[20];

	clrscr();

	printf("\n");
	do
	{
		printf(" Numarul de discuri (intre 1 si 200000) : ");
		scanf("%s", auxil1);
		nr = atol(auxil1);
		ltoa(nr, auxil2, 10);
		if (strcmp(auxil1, auxil2))
			nr = 200001;
	}
	while ((nr <= 0) || (nr > 200000));
	for (int i = 0; i < 3; i++)
		t[i] = new Tija();
}