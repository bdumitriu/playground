/* arbore partial de cost minim (algoritmul lui Kruskal)*/
#include <stdio.h>
#include <conio.h>
struct nod
{
   int x, y, c;
} u[100];

int neord(int m)
{
   int i;
   for (i= 1; i <= m-1; i= i+1)
      if (u[i].c > u[i+1].c)
	 return 1;
   return 0;
}

main()
{
   int a, b, c, m, n, i, j, k, l[100];
   struct nod aux;

   clrscr();
   printf("\n");

   printf(" Numarul de noduri : ");
   scanf("%i", &n);
   printf(" Numarul de muchii : ");
   scanf("%i", &m);
   for (i= 1; i <= m; i= i+1)
   {
      printf("  extremitatile muchiei %i : ", i);
      scanf("%i %i", &a, &b);
      printf("  costul muchiei %i : ", i);
      scanf("%i", &c);
      u[i].x= a;
      u[i].y= b;
      u[i].c= c;
   }
   for (i= 1; i <= n; i= i+1)
      l[i]= i;
   while (neord(m))
      for (i= 1; i < m; i= i+1)
	 if (u[i].c > u[i+1].c)
	    {
	       aux= u[i];
	       u[i]= u[i+1];
	       u[i+1]= aux;
	    }
   printf(" Arborele partial de cost minim are muchiile:\n");
   printf("  ");
   k= 0;
   i= 1;
   while (k < n-1)
   {
      if (l[u[i].x] != l[u[i].y])
	 {
	    k= k+1;
	    printf("[%i,%i] ", u[i].x, u[i].y);
	    int a1= l[u[i].y];
	    int a2= l[u[i].x];
	    for (j= 1; j <= n; j= j+1)
	       if (l[j] == a1)
		  l[j]= a2;
	 }
      i= i+1;
   }

   getch();
   return 0;
}