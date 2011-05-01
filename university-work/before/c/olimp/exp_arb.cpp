#include <stdio.h>
#include <conio.h>
#include <string.h>
#include <alloc.h>
#include <stdlib.h>
enum tip {op= 1, val};
enum oper {pl= 1, min, inm, imp};
struct info
{
   tip t;
   char o;
   int val;
};

struct nod
{
   info i;
   nod *s, *d;
};
char *x;

cr_arb(nod **n, int li, int ls)
{
   char *aux;

   int ord= 0;
   int ind= -1;
   for (int i= li; i < ls; i++)
   {
      if ((x[i] == '+') || (x[i] == '-'))
      {
	 ord= 2;
	 ind= i;
	 i= ls;
      }
      if ((x[i] == '*') || (x[i] == '/'))
      {
	 ord= 1;
	 ind= i;
      }
   }
   if (ind == -1)
      ind= li;
   *n= (nod *) malloc(sizeof(nod));
   if (ord == 0)
   {
      (*n)->i.t= val;
      *aux= x[ind];
      (*n)->i.val= atoi(aux);
      (*n)->s= NULL;
      (*n)->d= NULL;
   }
   else
   {
      (*n)->i.t= op;
      (*n)->i.o= x[ind];
/*
      switch (x[ind])
      {
	 case '+': n->i.o= pl; break;
	 case '-': n->i.o= min; break;
	 case '*': n->i.o= inm; break;
	 case '/': n->i.o= imp; break;
      }
*/
      cr_arb(&((*n)->s), li, ind);
      cr_arb(&((*n)->d), ind+1, ls);
   }
   return 0;
}

afis_arb(nod *n)
{
   if (n->s != NULL)
      afis_arb(n->s);
   if (n->i.t == op)
      printf("%c", n->i.o);
   if (n->i.t == val)
      printf("%i", n->i.val);
   if (n->d != NULL)
      afis_arb(n->d);
   return 0;
}

main()
{
   nod *n;
   char *aux;
   int nr;

   clrscr();
   printf("\n");

   x= (char *) malloc(50*sizeof(char));
   aux= (char *) malloc(sizeof(char));
//   n= (nod *) malloc(sizeof(nod));
   printf(" EXPRESIA : ");
   gets(x);
   *aux= x[0];
   if ((nr= atoi(aux)) == 0)
   {
      printf("  Expresia nu e corecta.");
      getch();
      exit;
   }
/*
   else
   {
      n->i.t= op;
      n->i.val= nr;
   }
*/
//   i= 0;
   cr_arb(&n, 0, strlen(x));
//   printf(" Valoarea expresiei este %f.", ());
   afis_arb(n);
   getch();

   return 0;
}