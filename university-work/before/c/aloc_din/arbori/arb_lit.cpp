#include <stdio.h>
#include <conio.h>

typedef struct _nod
{
   char c;
   _nod *s, *d;
} nod;

int pos;
char sir[51];

nod* constr()
{
   nod *n= new nod;
   n->s= n->d= NULL;

   n->c= sir[pos];
   pos+= 2;
   int nr_fii= sir[pos]-'0';
   pos+= 2;
   if (nr_fii == 2)
   {
      n->s= constr();
      n->d= constr();
   }
   else
      if (nr_fii == 1)
	 n->d= constr();

   return n;
}

preord(nod *n)
{
   if (n != NULL)
   {
       preord(n->s);
       printf("%c ", n->c);
       preord(n->d);
   }

   return 0;
}

adanc(nod *n)
{
   if (n == NULL)
      return 0;
   int as= adanc(n->s);
   int ad= adanc(n->d);

   return 1+((as > ad) ? as:ad);
}

tiparire(nod *n)
{
   if (n != NULL)
   {
      printf("%c", n->c);
      gotoxy(wherex()+2, wherey()+1);
      tiparire(n->s);
      tiparire(n->d);
      gotoxy(wherex()-3, wherey());
   }

   return 0;
}

void main()
{
   nod *n;

   clrscr();
   printf("\n");

   printf(" Sirul : ");
   gets(sir);
   pos= 0;
   n= constr();
   //preord(n);
   printf(" Adancimea este %i.", adanc(n));
   tiparire(n);

   getch();
   return;
}