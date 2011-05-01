#include <stdio.h>
#include <conio.h>
#include <math.h>
int s[9];

void scrie()
{
   int i, j;

   for (i= 1; i <= 8; i= i+1)
   {
      for (j= 0; j <= 8; j= j+1)
	 if (s[i] == j)
	    printf("R");
	 else
	    printf("*");
      printf("\n");
   }
   getch();
   printf("\n");
}

int ok(int i)
{
   int j;

   for (j= 1; j < i; j= j+1)
      if ((s[i] == s[j]) || (abs(i-j) == abs(s[i]-s[j])))
	 return 0;
   return 1;
}

main()
{
   int i;

   clrscr();
   printf("\n");

   i= 1;
   s[i]= 0;
   do
   {
		while (s[i] < 8)
		{
			s[i] = s[i]+1;
			if (ok(i))
			{
				if (i == 8)
					scrie();
				else
				{
					i = i+1;
					s[i] = 0;
				}
			}
		}
		i = i-1;
	}
	while (i > 0);
}