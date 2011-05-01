#include <stdio.h>
#include <conio.h>
#include <stdlib.h>
#include <math.h>

struct pt_s
{
   int nod, cost, nod_p;
};

int m, n, mat_i[21][15], mat_c[21][15], a, ax, ay, bx, by, k;
int gr[101][101];
pt_s s[101], ex[101];
int sol[101];

PeLinieSauPeDiagonala(int x1, int y1, int x2, int y2)
{
   if ((x1 == x2) || (y1 == y2) || (abs(x2-x1) == abs(y2-y1)))
      return 1;

   return 0;
}

ExistaVizibilitateDirecta(int x1, int y1, int x2, int y2)
{
   if ((x1 == x2) && (y1 == y2))
      return 0;
   if (!PeLinieSauPeDiagonala(x1, y1, x2, y2))
      return 0;
   float dist_dintre_relee, dist_de_vizib;

   dist_de_vizib= float(8400*(float(sqrt(float(mat_i[x1][y1])))));
   dist_dintre_relee= float(float(sqrt((float(x2-x1))*(float(x2-x1))+
		      (float(y2-y1))*(float(y2-y1))))*float(a));
   if (dist_de_vizib >= dist_dintre_relee)
      return 1;

   return 0;
}

CitireaDatelor()
{
   int i, j;

   printf(" Numarul de linii : ");
   scanf("%i", &m);
   printf(" Numarul de coloane : ");
   scanf("%i", &n);
   printf("  Latura patratului : ");
   scanf("%i", &a);
   for (i= 1; i <= m; i++)
      for (j= 1; j <= n; j++)
      {
	 /*
	 printf("  Cota %i %i : ", i, j);
	 scanf("%i", &mat_c[i][j]);
	 */
	 printf("  Antena de la cota %i %i : ", i, j);
	 scanf("%i", &mat_i[i][j]);
      }
   printf(" Coordonatele cotei de transmisie A : ");
   scanf("%i %i", &ax, &ay);
   printf(" Coordonatele cotei de receptie B : ");
   scanf("%i %i", &bx, &by);

   return 0;
}

Det_coord(int i, int *x1, int *y1)
{

   if (i%n != 0)
   {
      *x1= int(i/n)+1;
      *y1= i%n;
   }
   else
   {
      *x1= i/n;
      *y1= n;
   }

   return 0;
}

ConstruireaGrafului()
{
   int i, j, x1, y1, x2, y2;

   for (i= 1; i <= 160; i++)
      for (j= 1; j <= 160; j++)
	 gr[i][j]= 0;
   for (i= 1; i <= m*n; i++)
   {
      Det_coord(i, &x1, &y1);
      for (j= i; j <= m*n; j++)
      {
	 Det_coord(j, &x2, &y2);
	 if (ExistaVizibilitateDirecta(x1, y1, x2, y2))
	 {
	    gr[i][j]= 1;
	    gr[j][i]= 1;
	 }
      }
   }

   return 0;
}

ok(int i)
{
   for (int j= 1; j <= i; j++)
      if (ex[k].nod == s[j].nod)
	 return 0;
   for (j= 1; j < k; j++)
      if ((ex[k].nod == ex[j].nod) && (ex[k].cost >= ex[j].cost))
	 return 0;

   return 1;
}

Expandare(int i)
{
   for (int j= 1; j <= m*n; j++)
   {
      if (gr[s[i].nod][j] == 1)
      {
	 k++;
	 ex[k].nod= j;
	 ex[k].nod_p= s[i].nod;
	 ex[k].cost= s[i].cost+1;
	 if (!ok(i))
	    k--;
      }
   }

   return 0;
}

AlegereMinim()
{
   unsigned int min, ind;

   min= 30000;
   for (int j= 1; j <= k; j++)
      if (min > ex[j].cost)
      {
	 min= ex[j].cost;
	 ind= j;
      }
   for (j= 1; j <= k; j++)
      if (j != ind)
      {
	 if (ex[j].nod == ex[ind].nod)
	    for (int l= j; l < k; l++)
	    {
	       ex[l]= ex[l+1];
	       k--;
	       if (ind > l)
		  ind--;
	    }
      }

   return ind;
}

AflareaDrumuluiMinim(int nod_initial, int nod_final)
{
    int i, ind;

    i= 1;
    k= 0;
    s[i].nod= nod_initial;
    s[i].cost= 0;
    while (s[i].nod != nod_final)
    {
       Expandare(i);
       i++;
       ind= AlegereMinim();
       s[i]= ex[ind];
       for (int j= ind; j < k; j++)
	  ex[j]= ex[j+1];
       k--;
    }

    return i;
}

AfisareSolutie(int nr)
{
   int i, x, y;

   k= 1;
   sol[k]= s[nr].nod;
   i= nr;
   while (sol[k] != s[1].nod)
   {
       for (int j= 1; j <= nr; j++)
	  if (s[j].nod == s[i].nod_p)
	  {
	     i= j;
	     j= nr+1;
	  }
       k++;
       sol[k]= s[i].nod;
   }
   printf(" Sirul de relee este : ");
   for (i= k; i >= 1; i--)
   {
      Det_coord(sol[i], &x, &y);
      printf("(%i,%i) ", x, y);
   }
   printf("\n");

   return 0;
}

void main()
{
   int nr;

   clrscr();
   printf("\n");

   CitireaDatelor();
   ConstruireaGrafului();
   nr= AflareaDrumuluiMinim(((ax-1)*n+ay), ((bx-1)*n+by));
   AfisareSolutie(nr);

   getch();
   return;
}