#include <stdio.h>
#include <conio.h>

struct pt_s
{
   int x, y;
};

int a[101][101], m, n, sol, aux;
int x[5]= {0, 0, 1, 0, -1};
int y[5]= {0, -1, 0, 1, 0};
pt_s s[101];

gata()
{
   for (int i= 1; i <= m; i++)
      for (int j= 1; j <= n; j++)
	 if (a[i][j] == 1)
	    return 0;

   return 1;
}

ok(int i)
{
   if ((s[i].x < 1) || (s[i].x > n) || (s[i].y < 1) || (s[i].y > m))
      return 0;

   return 1;
}

back(int pas)
{
   int ii, jj;

   for (int k= 1; k <= 4; k++)
   {
      s[pas].x= s[pas-1].x+x[k];
      s[pas].y= s[pas-1].y+y[k];
      if ((a[s[pas].x][s[pas].y] == 1) && (ok(pas)))
      {
	 ii= s[pas].x;
	 jj= s[pas].y;
	 a[ii][jj]= 0;
	 if (gata())
	    aux= 1;
	 else
	    back(pas+1);
	 //a[ii][jj]= 1;
      }
   }

   return aux;
}

cit_date()
{
   int i, j;

   printf(" Numarul de linii ale matricei : ");
   scanf("%i", &m);
   printf(" Numarul de coloane ale matricei : ");
   scanf("%i", &n);
   for (i= 1; i <= m; i++)
     for (j= 1; j <= n; j++)
     {
	printf("  elementul %i %i : ", i, j);
	scanf("%i", &a[i][j]);
     }

   return 0;
}

main()
{
   int i, j;

   clrscr();
   printf("\n");

   int test= 0;
   cit_date();
   for (i= 1; i <= m; i++)
     for (j= 1; j <= n; j++)
	if (a[i][j] == 1)
	{
	   s[1].x= i;
	   s[1].y= j;
	   a[i][j]= 0;
	   if ((sol= back(2)) == 1)
	   {
	      printf("Da !");
	      test= 1;
	   }
	   else
	   {
	      printf("Nu !");
	      test= 1;
	   }
	   j= n+1;
	   i= m+1;
	}
   if (!test)
      printf("Nu !");

   getch();
   return 0;
}