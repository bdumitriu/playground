#include <stdio.h>
#include <conio.h>
#include <stdlib.h>
int c[7]= {0, 1, 2, 3, 4, 5, 6};//{alb, galben, rosu, verde, albastru, negru}
int s[4], i;

scrie()
{
   for (int j= 1; j <= 3; j++)
      switch (s[j])
      {
	 case 1: printf("alb "); break;
	 case 2: printf("galben "); break;
	 case 3: printf("rosu "); break;
	 case 4: printf("verde "); break;
	 case 5: printf("albastru "); break;
	 case 6: printf("negru "); break;
      }
   printf("\n");

   return 0;
}

ok()
{
   if ((i == 2) && (s[i] != 2) && (s[i] != 4))
      return 0;
   for (int j= 1; j < i; j++)
      if (s[j] == s[i])
	 return 0;

   return 1;
}

backtraking()
{
   i= 1;
   s[i]= 0;
   do
   {
      while (s[i] < 6)
      {
	 s[i]++;
	 if (ok())
	    if (i == 3)
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

   backtraking();

   getch();
   return 0;
}