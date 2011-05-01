#include <stdio.h>
#include <conio.h>
#include <math.h>

void citirea_datelor(int *m, int *n, int ret[4][4])
{
   int i, j, a;
   printf(" n = ");
   scanf("%i", n);
   printf(" m = ");
   scanf("%i", m);
   for (i= 1; i <= *m; i= i+1)
      for (j =1; j <= *n; j= j+1)
      {
	 printf(" Cota %i %i : ", i, j);
	 scanf("%i", &ret[i][j]);
      }
}

void initializarea_matricei_de_adiacenta(float gr[10][10])
{
   int i, j;

   for (i= 1; i <= 9; i= i+1)
      for (j= 1; j <= 9; j= j+1)
	 gr[i][j]= 32000;
}

int iz1(int a, int ret[4][4], int m, int n)
{
   enum
   {
      x1= -1, x2= -1, x3= 0, x4= 1, x5= 1, x6= 1, x7= 0, x8= -1
   } x;
   enum
   {
      y1= 0, y2= -1, y3= -1, y4= -1, y5= 0, y6= 1, y7= 1, y8= 1
   } y;
   int i, j, l, c;

   c= a%n;
   a= a-c+n;
   l= a/n;
   for (i= 0; i <= 7; i= i+1)
      if (((c+x1+i > 0) && (c+x1+i <= n)) && ((l+y1+i > 0) && (l+y1+i <= m)))
	 if (ret[l+y1+i][c+x1+i] != 0) return 0;
   return 1;
}

int iz2(int a, int ret[4][4], int m, int n)
{
   enum
   {
      x1= -2, x2= -2, x3= 0, x4= 2, x5= 2, x6= 2, x7= 0, x8= -2
   } x;
   enum
   {
      y1= 0, y2= -2, y3= -2, y4= -2, y5= 0, y6= 2, y7= 2, y8= 2
   } y;
   int i, j, l, c;

   c= a%n;
   a= a-c+n;
   l= a/n;
   for (i= 0; i <= 7; i= i+1)
      if (((c+x1+i > 0) && (c+x1+i <= n)) && ((l+y1+i > 0) && (l+y1+i <= m)))
	 if (ret[l+y1+i][c+x1+i] != 0) return 1;
   return 0;
}

void legaturi1(int a, int ret[4][4], int m, int n, float gr[10][10])
{
   enum
   {
      x1= -1, x2= -1, x3= 0, x4= 1, x5= 1, x6= 1, x7= 0, x8= -1
   } x;
   enum
   {
      y1= 0, y2= -1, y3= -1, y4= -1, y5= 0, y6= 1, y7= 1, y8= 1
   } y;
   int i, j, l, c;

   c= a%n;
   a= a-c+n;
   l= a/n;
   for (i= 0; i <= 7; i= i+1)
      if (((c+x1+i > 0) && (c+x1+i <= n)) && ((l+y1+i > 0) && (l+y1+i <= m)))
	 if (ret[l][c] == ret[l+y1+i][c+x1+i])
	    gr[(l+y1+i-1)*n+(c+x1+i)][a]= 0.5;
	 else
	    gr[(l+y1+i-1)*n+(c+x1+i)][a]= abs(ret[l+y1+i][c+x1+i]-ret[l][c]);
}

void legaturi2(int a, int ret[4][4], int m, int n, float gr[10][10])
{
   enum
   {
      x1= -2, x2= -2, x3= 0, x4= 2, x5= 2, x6= 2, x7= 0, x8= -2
   } x;
   enum
   {
      y1= 0, y2= -2, y3= -2, y4= -2, y5= 0, y6= 2, y7= 2, y8= 2
   } y;
   int i, j, l, c;

   c= a%n;
   a= a-c+n;
   l= a/n;
   for (i= 0; i <= 7; i= i+1)
      if (((c+x1+i > 0) && (c+x1+i <= n)) && ((l+y1+i > 0) && (l+y1+i <= m)))
	 if (ret[l][c] == ret[l+y1+i][c+x1+i])
	    gr[(l+y1+i-1)*n+(c+x1+i)][a]= 1;
	 else
	    gr[(l+y1+i-1)*n+(c+x1+i)][a]= 2*abs(ret[l+y1+i][c+x1+i]-ret[l][c]);
}

void posibilitati_de_legare_a_stalpilor(int m, int n, int ret[4][4], float gr[10][10])
{
   int i, j;

   for (i= 1; i <= n; i= i+1)
      for (j= 1; j <= n; j= j+1)
	 if (iz1((i-1)*n+j, ret, m, n))
	    if (iz2((i-1)*n+j, ret, m, n))
	       legaturi2((i-1)*n+j, ret, m, n, gr);
	 else
	    legaturi1((i-1)*n+j, ret, m, n, gr);
}

int detl(int a, int n)
{
   a= a-(a%n)+n;
   return a/n;
}

void afisarea_legaturilor(float gr[10][10], int m, int n)
{
   int i, j;

   for (i= 1; i <= m*n; i= i+1)
      for (j= 1; j <= m*n; j= j+1)
	 if (gr[i][j] != 32000)
	 {
	    printf(" Legatura este intre varfurile ");
	    printf("(%i,%i) (%i,%i) cu costul %i.\n", detl(i,n), i%n, detl(j,n), j%n);
	 }
}

main()
{
   int i, j, m, n, ret[4][4];
   float gr[10][10];

   clrscr();
   printf("\n");

   citirea_datelor(&m, &n, ret);
   initializarea_matricei_de_adiacenta(gr);
   posibilitati_de_legare_a_stalpilor(m, n, ret, gr);
   afisarea_legaturilor(gr,m,n);

   return 0;
}