#include <stdio.h>
#include <conio.h>

struct pt_s
{
   int x, y;
};

int a[101][101], m, n, si, sj;
int x[5]= {0, 0, 1, 0, -1};
int y[5]= {0, -1, 0, 1, 0};
pt_s s[101];

scrie(int i)
{
   for (int j= 1; j <= i; j++)
      printf("{%i,%i} ", s[j].x, s[j].y);
   printf("\n");

   return 0;
}

ok(int i)
{
   if (a[s[i-1].x][s[i-1].y] < a[s[i].x][s[i].y])
      return 0;
   if ((s[i].x < 1) || (s[i].x > n) || (s[i].y < 1) || (s[i].y > m))
      return 0;

   return 1;
}

back(int i)
{
   int j;

   for (j= 1; j <= 4; j++)
   {
      s[i].x= s[i-1].x+x[j];
      s[i].y= s[i-1].y+y[j];
      if (ok(i))
	 if ((s[i].x == 1) || (s[i].x == n) || (s[i].y == 1) || (s[i].y == m))
	 {
	    scrie(i);
	    back(i+1);
	 }
	 else
	    back(i+1);
   }

   return 0;
}

main()
{
   int i, j;

   clrscr();
   printf("\n");

   printf(" m = ");
   scanf("%i", &m);
   printf(" n = ");
   scanf("%i", &n);
   for (i= 1; i <= m; i++)
      for (j= 1; j <= n; j++)
      {
	 printf(" cota %i %i : ", i, j);
	 scanf("%i", &a[i][j]);
      }
   printf(" coordonatele la care se afla schiorul : ");
   scanf("%i %i", &si, &sj);
   s[1].x= si;
   s[1].y= sj;
   back(2);

   return 0;
}