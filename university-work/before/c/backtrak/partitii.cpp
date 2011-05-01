/* program pentru aflarea partiilor unui numar natural */
#include <stdio.h>
#include <conio.h>
int s[100], i;

scrie(int i, int n)
{
   int j;

   printf("%i = ", n);
   for (j= 1; j <= i-1; j++)
      printf("%i+", s[j]);
   printf("%i\n", s[i]);
   getch();

   return 0;
}

int sum(int i)
{
   int j, su;

   su= 0;
   for (j= 1; j <= i; j++)
      su= su+s[j];

   return su;
}

ok(int i, int n)
{
   int j;

   for (j= 1; j <= i-1; j++)
      if (s[j] > s[j+1])
	 return 0;
   if (sum(i) > n)
      return 0;

   return 1;
}

part(int i, int n)
{
   int j;

   for(j= 1; j <= n; j++)
   {
      s[i]= j;
      if (ok(i, n))
	 if (sum(i) == n)
	    scrie(i, n);
	 else
	    part(i+1, n);
   }

   return 0;
}

main()
{
   int n;

   clrscr();
   printf("\n");

   printf(" Numarul : ");
   scanf("%i", &n);
   i= 1;
   part(i, n);

   return 0;
}