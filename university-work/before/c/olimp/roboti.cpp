#include <stdio.h>
#include <conio.h>
#include <stdlib.h>

struct nod
{
   char c;
   nod *leg;
};

nod *varf[50], *p, *q;
int i, j, n, nr[50];
char c;

int caut_max()
{
   int x, y;

   x= 0;
   for (i= 1; i <= n; i++)
   {
      p= varf[i];
      y= 0;
      while (p != NULL)
      {
	 y++;
	 p= p->leg;
      }
      if (x < y)
	 x= y;
   }

   return x;
}

afisare(int x)
{
   int y;

   for (i= 1; i <= n; i++)
   {
      p= varf[i];
      y= 0;
      while (p != NULL)
      {
	 y++;
	 p= p->leg;
      }
      j= 0;
      p= varf[i];
      while (p != NULL)
      {
	 j++;
	 gotoxy(3*i, x-y+j);
	 printf("%c", p->c);
	 p= p->leg;
      }
   }

   return 0;
}

citire_date()
{
   printf(" Acum va voi ruga sa introduceti starea initiala:\n");
   printf("  - numarul de constructii de pe masa : ");
   scanf("%i", &n);
   i= 0;
   while (i < n)
   {
      i++;
      printf("    - numarul de cuburi din constructia %i : ", i);
      scanf("%i", &nr[i]);
      q= NULL;
      printf("     introduceti cuburile din constructia %i (incepand cu baza):\n", i);
      for (j= 1; j <= nr[i]; j++)
      {
	 p= (nod *) malloc(sizeof(nod));
	 if (j == nr[i])
	    varf[i]= p;
	 printf("      - cubul %i : ", j);
	 scanf("%c", &c);
	 scanf("%c", &c);
	 p->leg= q;
	 p->c= c;
	 q= p;
      }
   }

   return 0;
}

void main()
{
   int x;

   clrscr();
   printf("\n");

   eti:
   citire_date();
   x= caut_max();
   clrscr();
   afisare(x);
   printf("\n Aceasta este configuratia buna (d/n) ? ");
   scanf("%c", &c);
   scanf("%c", &c);
   if (c == 'n')
   {
      for (i= 1; i <= n; i++)
      {
	 p= varf[i];
	 while (p != NULL)
	 {
	    q= p;
	    p= p->leg;
	    delete q;
	 }
      }
      goto eti;
   }

   return;
}