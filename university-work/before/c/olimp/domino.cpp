#include <stdio.h>
#include <conio.h>

struct pt_a
{
   int x, y;
};
struct pt_s
{
   int n, lib;
};

pt_a a[101];
pt_s s[101], s1[101];
int n;

scrie(int i)
{
   for (int j= 1; j <= i; j++)
   {
      gotoxy(j, wherey()+1);
      if (s[j].lib == 1)
	 printf("%i%i", a[s[j].n].y, a[s[j].n].x);
      if (s[j].lib == 2)
	 printf("%i%i", a[s[j].n].x, a[s[j].n].y);
   }
}

ok(int i, int *j)
{
   if (i == 1)
      return 1;
   for (int k= 1; k < i; k++)
   {
      if (s[k].n == s[i].n)
	 return 0;
   }
   if (i == 2)
   {
      if (a[s[1].n].x == a[s[2].n].x)
      {
	 (*j)= 2;
	 s[1].lib= 1;
	 return 1;
      }
      if (a[s[1].n].x == a[s[2].n].y)
      {
	 (*j)= 1;
	 s[1].lib= 1;
	 return 1;
      }
      if (a[s[1].n].y == a[s[2].n].x)
      {
	 (*j)= 2;
	 s[1].lib= 2;
	 return 1;
      }
      if (a[s[1].n].y == a[s[2].n].y)
      {
	 (*j)= 1;
	 s[1].lib= 2;
	 return 1;
      }
   }
   if (s[i-1].lib == 1)
   {
      if (a[s[i-1].n].x == a[s[i].n].x)
      {
	 (*j)= 2;
	 return 1;
      }
      if (a[s[i-1].n].x == a[s[i].n].y)
      {
	 (*j)= 1;
	 return 1;
      }
   }
   if (s[i-1].lib == 2)
   {
      if (a[s[i-1].n].y == a[s[i].n].x)
      {
	 (*j)= 2;
	 return 1;
      }
      if (a[s[i-1].n].y == a[s[i].n].y)
      {
	 (*j)= 1;
	 return 1;
      }
   }

   return 0;
}

back()
{
  int i, j, tst, max;

  s[1].n= 0;
  i= 1;
  tst= 0;
  max= 0;
  do
  {
     while (s[i].n < n)
     {
	s[i].n++;
	if (ok(i, &j))
	{
	   if (i > 1)
	      s[i].lib= j;
	   if (i == n)
	   {
	      scrie(n);
	      tst= 1;
	   }
	   else
	   {
	      i++;
	      s[i].n= 0;
	   }
	}
     }
     if (max < i)
     {
	max= i;
	for (int j= 1; j <= i; j++)
	   s1[j]= s[j];
     }
     s[i].lib= 0;
     i--;
  }
  while ((i > 0) || (tst));
  if (tst == 0)
  {
     for (int j= 1; j <= max; j++)
	s[j]= s1[j];
     scrie(max);
  }

  return 0;
}

cit_dat()
{
   int i;

   printf(" Numarul de piese : ");
   scanf("%i", &n);
   for (i= 1; i <= n; i++)
   {
      printf("  - numerele de pe piesa %i : ", i);
      scanf("%i %i", &a[i].x, &a[i].y);
   }

   return 0;
}

void main()
{
   int i;

   clrscr();
   printf("\n");

   cit_dat();
   back();

   getch();
   return;
}