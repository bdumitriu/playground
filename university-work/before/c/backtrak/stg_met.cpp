#include <stdio.h>
#include <conio.h>
#include <stdlib.h>
int c[6]= {1, 2, 3, 4, 5, 6};
int s[100], i= 1;

proceseaza_sol()
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

avanseaza()
{
   i++;
   /*
   do
   {
   */
      s[i]++;
      if ((i == 2) && (s[i] != 2) && (s[i] != 4))
	 return 0;
      for (int j= 1; j < i; j++)
	 if (s[j] == s[i])
	    return 0;

      return 1;
   /*
   }
   while (s[i] < 6);
   */
}

backtraking()
{
   while (i > 0)
   {
      while (avanseaza())
      {
	 if (i == 3)
	    proceseaza_sol();
      }
      i--;
   }

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