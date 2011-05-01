#include <stdio.h>
#include <conio.h>
#include "c:\bc31\work\surse\new\clase\graf.h"

Graf::Graf(int x, int y)
{
   for (int i= 0; i < x; i++)
      t[i]= new int[x];
   for (i= 0; i < x; i++)
      for (int j= 0; j < x; j++)
	 t[i][j]= 0;
   tip= y;
   n= x;
}

Graf::~Graf()
{
   for (int i= 0; i < n; i++)
      delete [] t[i];
}

void Graf::adm(int x, int y, int z)
{
   t[x][y]= z;
   if (tip == 0)
      t[y][x]= z;
}

void Graf::scm(int x, int y)
{
   t[x][y]= 0;
   if (tip == 0)
      t[y][x]= 0;
}

void Graf::bf(int x)
{
   int viz[100], s[100], p, u, i;

   for (i= 0; i < n; i++)
      viz[i]= 0;
   viz[x]= 1;
   printf("\n Graful in parcugerea BreadthFirst : %i ", x);
   s[0]= x;
   p= 0;
   u= 1;
   while (p <= u)
   {
      for (i= 0; i < n; i= i+1)
 	 if (((t[s[p]][i] == 1) || (t[i][s[p]] == 1)) && (viz[i] == 0))
	 {
	    viz[i]= 1;
	    printf("%i ", i);
	    s[u++]= i;
	 }
      p= p+1;
   }
   getch();
}

void Graf::df(int x)
{
   int viz[100], s[100], p, i, k;

   for (i= 0; i < n; i++)
      viz[i]= 0;
   printf("\n Graful in parcurgerea DepthFirst : %i ", x);
   viz[x]= 1;
   s[0]= x;
   p= 1;
   while (p > 0)
   {
      k= 0;
      while ((k < n) && ((t[s[p-1]][k] == 0) || ((t[s[p-1]][k] == 1) && (viz[k] == 1))))
	 k= k+1;
      if (k == n)
      {
	 p--;
      }
      else
      {
	 s[p++]= k;
	 viz[k]= 1;
	 printf("%i ", k);
      }
   }

   getch();
}