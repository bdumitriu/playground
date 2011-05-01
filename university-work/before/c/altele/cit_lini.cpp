#include <stdio.h>
#include <conio.h>
#include <string.h>
#include <stdlib.h>

main()
{
   int m, n, gr[8][8];
   char s[100];
   char *a, *p;
   int x[10][10];
   FILE *f;

   clrscr();

   f= fopen("test.txt", "r");
   int i= 0;
   int k= 0;
   int max= 0;
   while (fgets(s, 100, f))
   {
      i++;
      strcat(s, " ");
      a= s;
      k= 0;
      while (p= strtok(a, " "))
      {
	 k++;
	 x[i][k]= atoi(p);
	 a= NULL;
      }
      if (k > max)
	 max= k;
   }
   m= i;
   n= max;
   fclose(f);
   for (i= 1; i <= m; i++)
   {
      for (k= 1; k <= n; k++)
		printf("%i ", x[i][k]);
      printf("\n");
   }

   getch();
   return 0;
}