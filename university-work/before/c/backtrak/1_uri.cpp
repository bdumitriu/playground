#include <stdio.h>
#include <conio.h>

int a[101][101], m, n, k;
int x[5]= {0, -1, 0, 1, 0};
int y[5]= {0, 0, -1, 0, 1};

Back(int i, int ii, int jj)
{
   int j, iii, jjj;

   for (j= 1; j <= 4; j++)
   {
      iii= ii+x[j];
      jjj= jj+y[j];
      if ((i >= 1) || (i <= m) || (j >= 1) || (j <= n))
	 if (a[iii][jjj] == 1)
	 {
	    a[iii][jjj]= k;
	    back(i+1, iii, jjj);
	 }
   }

   return 0;
}

Afisare()
{
   int i, j;

   for (i= 1; i <= m; i++)
   {
      for (j= 1; j <= n; j++)
	 printf("%i ", a[i][j]);
      printf("\n");
   }

   return 0;
}

CitireDate()
{
   int i, j;

   printf(" Numarul de linii : ");
   scanf("%i", &m);
   printf(" Numarul de coloane : ");
   scanf("%i", &n);
   for (i= 1; i <= m; i++)
      for (j= 1; j <= n; j++)
      {
	 printf(" - elementul [%i,%i] : ", i, j);
	 scanf("%i", &a[i][j]);
      }

   return 0;
}

main()
{
   int i, j;

   clrscr();
   printf("\n");

   CitireDate();
   k= 1;
   for (i= 1; i <= m; i++)
      for (j= 1; j <= n; j++)
	 if (a[i][j] == 1)
	 {
	    k++;
	    a[i][j]= k;
	    Back(2, i, j);
	 }
   Afisare();

   return 0;
}