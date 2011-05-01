#include <stdio.h>
#include <conio.h>
#include <stdlib.h>
int n, i, s[1000];

scrie()
{
   printf("%i = ", n);
   for (int j= 1; j < i; j++)
      printf("%i+",s[j]);
   printf("%i.\n", s[i]);

   return 0;
}

int suma()
{
   int sm;

   sm= 0;
   for (int j= 1; j <= i; j++)
      sm+= s[j];

   return sm;
}

ok()
{
   if ((i >= 2) && (s[i] < s[i-1]))
      return 0;
   if (suma() > n)
      return 0;

   return 1;
}

backtraking()
{
   i= 1;
   s[i]= 0;
   do
   {
      while (s[i] < n)
      {
      s[i]++;
      if (ok())
	 if (suma() == n)
	    scrie();
	 else
	 {
	    i++;
	    s[i]= 0;
	 }
      }
      i--;
   }
   while (i > 0);

   return 0;
}

main()
{
   clrscr();
   printf("\n");

   printf(" Numarul ale carui partitii doriti sa le aflati : ");
   scanf("%i", &n);
   i= 0;
   backtraking();

   getch();
   return 0;
}