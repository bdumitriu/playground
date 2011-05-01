/* drum minim intre doua noduri */
#include <stdio.h>
#include <conio.h>

main()
{
   struct nod              /* id - identificatorul (numarul) nodul curent. */
	  {                /* n  - nodul prin care trece respectivul drum. */
	     int id, n, c; /* c  - costul drumului de la nodul initial */
	  } g[20], s[20];  /*      pana la nodul curent. */
   int c, i, j, m, n, n1, n2;     /* in g se vor memora nodurile expandate. */
   int gr[10][10];                /* in s se vor memora nodurile finale. */
   int min, min_in, x, y;         /* in gr se va memora matricea grafului. */
   int test, cost, f[20];

   clrscr();
   printf("\n");

   printf(" Introduceti numarul de noduri al grafului : ");
   scanf("%i", &n);

   for (i= 1; i <= n; i= i+1)   /* initializarea matricii cu 0 */
      for (j= 1; j <= n; j= j+1)
	 gr[i][j]= 0;

   printf(" Introduceti numarul de muchii al grafului : ");
   scanf("%i", &m);
   for (i= 1; i <= m; i= i+1)             /* citirea datelor */
   {
      printf("  Muchia %i : ", i);
      scanf("%i %i", &n1, &n2);
      printf("  Costul muchiei %i %i : ", n1, n2);
      scanf("%i", &c);
      gr[n1][n2]= c;
      gr[n2][n1]= c;
   }

   for (i= 1; i <= n; i= i+1)         /* initializarea valorilor muchiilor */
      for (j= 1; j <= n; j= j+1)      /* inexistente cu infinit */
	 if (gr[i][j] == 0)
	    gr[i][j]= 32000;

   printf("\n nodul initial : ");
   scanf("%i", &n1);
   printf(" nodul final : ");
   scanf("%i", &n2);
   printf("\n");
   x= 1;
   y= 0;
   s[x].id= n1;
   s[x].c= 0;
   while (s[x].id != n2)
   {
      for (i= 1; i <= n; i= i+1)
	 if (gr[i][s[x].id] != 32000)
	 {
	    y= y+1;
	    test= 1;
	    g[y].id= i;
	    g[y].n= s[x].id;
	    g[y].c= s[x].c+gr[i][s[x].id];
	    for (j= 1; j < y; j= j+1)
	       if ((g[j].id == g[y].id) && (g[j].c < g[y].c))
		  test= 2;
	    for (j= 1; j <= x; j= j+1)
	       if (s[j].id == g[y].id)
		  test= 2;
	    if (test == 2)
	       y= y-1;
	 }
      x= x+1;
      min= g[1].c;
      min_in= 1;
      for (j= 2; j <= y; j= j+1)
	 if (g[j].c < min)
	 {
	    min= g[j].c;
	    min_in= j;
	 }
      s[x]= g[min_in];
      for (j= min_in; j < y; j= j+1)
	 g[j]= g[j+1];
      y= y-1;
   }

   for (i= 0; i <= 20; i= i+1)
      f[i]= 0;
   cost= s[x].c;
   y= 1;
   f[1]= s[x].id;
   min= x;
   do
   {
      for (i= 1; i < min; i= i+1)
	 if (s[i].id == s[min].n)
	 {
	    min= i;
	    break;
	 }
      y= y+1;
      f[y]= s[min].id;
   }
   while (f[y] != n1);

   printf(" Nodurile prin care se trece sunt : ");
   for (i= y; i>=1; i= i-1)
      printf("%i ", f[i]);
   printf("\n");
   printf(" Costul drumului este %i.", cost);

   getch();
   return 0;
}
