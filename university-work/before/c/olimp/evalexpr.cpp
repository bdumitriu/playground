/* Program de evaluare a unei expresii */
#include <stdio.h>
#include <conio.h>
#include <string.h>
#include <alloc.h>
#include <stdlib.h>

int i;
enum tip {op= 1, val};
enum oper {pl= 1, min, inm, imp};
struct info
{
   tip t;
   oper o;
   int val;
};

struct nod
{
   info i;
   nod *s, *d;
};
char *x, *aux;
nod *n;
int nr;

factor()
{
   *aux= x[i];
   if ((nr= atoi(aux)) == 0)
   {
      printf("  Expresia nu e corecta.");
      getch();
      exit;
   }
   else
      return nr;
}

float termen()
{
   float fs, fd;
   char c;
   oper o;

   fs= factor();
   if ((x[i+1] == '*') || (x[i+1] == '/'))
   {
      i++;
      if (x[i] == '*')
	 c= '*';
      else
	 c= '/';
      i++;
      fd= factor();
      if (c == '*')
	 return fs*fd;
      else
	 return fs/fd;
   }
   else
      return fs;
}

float exp()
{
   float ts, td;
   char c;
   oper o;

   ts= termen();
   if ((x[i+1] == '+') || (x[i+1] == '-'))
   {
      i++;
      if (x[i] == '+')
	 c= '+';
      else
	 c= '-';
      i++;
      td= factor();
      if (c == '+')
	 return ts+td;
      else
	 return ts-td;
   }
   else
      return ts;
}

main()
{

   clrscr();
   printf("\n");
   x= (char *) malloc(50*sizeof(char));
   aux= (char *) malloc(sizeof(char));
   n= (nod *) malloc(sizeof(nod));
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
   i= 0;
   printf(" Valoarea expresiei este %f.", exp());
   getch();

   return 0;
}