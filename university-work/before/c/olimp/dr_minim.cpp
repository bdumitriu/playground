/* algoritm pt. determinarea matricei drumurilor de cost minim (Roy-Floyd) */
#include <stdio.h>
#include <conio.h>
#include <values.h>

unsigned int min(unsigned int value1, unsigned int value2)
{
   return ( (value1 < value2) ? value1 : value2);
}

void cit_dat(unsigned int *m, unsigned int *n, unsigned int gr[20][20])
{
   int i, j, a, b, c;
   printf(" Numarul de noduri ale grafului : ");
   scanf("%i", n);
   printf(" Numarul de arcuri ale grafului : ");
   scanf("%i", m);
   for (i= 1; i <= *n; i= i+1)
      for (j= 1; j <= *n; j= j+1)
	 if (i == j)
	    gr[i][j]= 0;
	 else
	    gr[i][j]= 35000;
   for (i= 1; i <= *m; i= i+1)
   {
      printf("  arcul %i : ", i);
      scanf("%i %i", &a, &b);
      printf("  costul arcului %i %i : ", a, b);
      scanf("%i", &c);
      gr[a][b]= c;
      gr[b][a]= c;
   }
}

main()
{
   unsigned int gr[20][20], i, j, k, m, n;

   clrscr();
   printf("\n");

   cit_dat(&m, &n, gr);
   for (k= 1; k <= n; k++)
      for (i= 1; i <= n; i++)
	 for (j= 1; j <= n; j++)
	    gr[i][j]= min(gr[i][j], gr[i][k]+gr[k][j]);
   printf(" Matricea drumurilor de cost minim este:\n");
   for (i= 1; i <= n; i++)
   {
      for (j= 1; j <= n; j++)
	 printf(" %i", gr[i][j]);
      printf("\n");
   }
   getch();

   return 0;
}