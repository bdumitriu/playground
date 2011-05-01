#include <stdio.h>
#include <conio.h>
#include <stdlib.h>
#include <string.h>

main ()
{
   int x;
   char y[100];
   char z[100];

   clrscr();

   do
   {
      printf("Introduceti valoarea : ");
		scanf("%s", y);
		x = atoi(y);
		itoa (x, z, 10);
		if (strcmp(z, y)  || !x) x = 101;
   }
   while ((x<0) || (x>100));

   printf("%i",x);
   getch();
}

