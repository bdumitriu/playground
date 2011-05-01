/* algoritm pt. determinarea matricei drumurilor */
#include <stdio.h>
#include <conio.h>

void cit_dat(int *m, int *n, int gr[20][20])
{
   int i, j, a, b;
   printf(" Numarul de noduri ale grafului : ");
   scanf("%i", n);
   printf(" Numarul de arcuri ale grafului : ");
   scanf("%i", m);
   for (i= 1; i <= *n; i= i+1)
      for (j= 1; j <= *n; j= j+1)
	 gr[i][j]= 0;
   for (i= 1; i <= *m; i= i+1)
   {
      printf("  arcul %i : ", i);
      scanf("%i %i", &a, &b);
      gr[a][b]= 1;
   }
}

main()
{
   int gr[20][20], i, j, k, m, n;

   clrscr();
   printf("\n");

   cit_dat(&m, &n, gr);
   for (k= 1; k <= n; k++)
      for (i= 1; i <= n; i++)
	 for (j= 1; j <= n; j++)
	    if (gr[i][j] == 0)
	       gr[i][j]= gr[i][k]*gr[k][j];
   printf(" Matricea drumurilor este (1 daca este drum intre i si j, 0 altfel):");
   for (i= 1; i <= n; i++)
   {
      for (j= 1; j <= n; j++)
	 printf(" %i", gr[i][j]);
      printf("\n");
   }
   getch();

   return 0;
}