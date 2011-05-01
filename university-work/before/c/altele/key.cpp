#include <stdio.h>
#include <conio.h>

void main()
{
	char c1, c2;

	clrscr();
	printf("\n");

	do
	{
		printf(" Apasati o tasta (ESC pt. iesire - cod 27):        ");
		c1 = getch();
		if (c1 == 0)
		{
			c2 = getch();
			printf(" 0 + %i \n", int(c2));
		}
		else
			printf("     %i \n", int(c1));
	}
	while (c1 != 27);

}