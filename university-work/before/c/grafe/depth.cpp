/* algoritm de parcurge a unui graf in adancime */
#include <stdio.h>
#include <conio.h>

void cit_dat(int *m, int *n, int gr[20][20], int viz[20])
{
   int i, j, a, b;
   printf(" Numarul de noduri ale grafului : ");
   scanf("%i", n);
   printf(" Numarul de muchii ale grafului : ");
   scanf("%i", m);
   for (i= 1; i <= *n; i= i+1)
   {
      for (j= 1; j <= *n; j= j+1)
	 gr[i][j]= 0;
      viz[i]= 0;
   }
   for (i= 1; i <= *m; i= i+1)
   {
      printf("  muchia %i : ", i);
      scanf("%i %i", &a, &b);
      gr[a][b]= 1;
      gr[b][a]= 1;
   }
}

main()
{
   int i, k, m, n, p, v, gr[20][20], viz[20], s[20];

   clrscr();
   printf("\n");

   cit_dat(&m, &n, gr, viz);

   printf("  Varful de plecare : ");
   scanf("%i", &v);
   printf(" Graful este : ");
   printf("%i ", v);
   viz[v]= 1;
   s[1]= v;
   p= 1;
   while (p >= 1)
   {
      k= 1;
      while ((k <= n) && ((gr[s[p]][k] == 0) || ((gr[s[p]][k] == 1) && (viz[k] == 1))))
	 k= k+1;
      if (k == n+1)
	 p= p-1;
      else
      {
	 p++;
	 s[p]= k;
	 viz[k]= 1;
	 printf("%i ", k);
      }
   }

   getch();
   return 0;
}