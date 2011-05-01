#include <stdio.h>
#include <conio.h>
#include <math.h>

int primul()
{
   return 1;
}

int urmatorul(int i)
{
   if (i == 8)
      return 0;
   else
      return i+1;
}

int condcontin(int x[8], int k)
{
   int i;

   for (i= 1; i <= k-1; i= i+1)
      if ((x[k] == x[i]) || (k-i == abs(x[k]-x[i])))
	 return 0;
   return 1;
}

rezultate(int *nrsol, int x[8], int n)
{
   int i, c;

   clrscr();
   printf("          Solutia nr. %i\n", *nrsol);
   printf("Tabla de sah        Pozitia reginei :\n");
   nrsol++;
   for (i= 1; i <= n; i++)
   {
      printf("\n");
      printf("  ");
      for (c= 1; c <= n; c++)
	 if (c == x[i])
	    printf("*");
	 else
	    printf("=");
      printf("            %i %3i    ", i, x[i]);
   }
   getch();

   return 0;
}

backtracking(int x[8], int n)
{
   int nrsol, k;

   k= 1;
   x[1]= primul();
   do
   {
      while ((k < n) && (condcontin(x, k)))
      {
	 k++;
	 x[k]= primul();
      }
      if ((k == n) && (condcontin(x, k)))
	 rezultate(&nrsol, x, n);
      while ((k > 0) && (urmatorul(x[k]) == 0))
	 k--;
      if (k > 0)
	 x[k]= urmatorul(x[k]);
   }
   while (k > 0);

   return 0;
}

main()
{
   int x[8];

   backtracking(x,8);

   return 0;
}