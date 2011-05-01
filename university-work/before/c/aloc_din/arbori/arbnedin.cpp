#include <stdio.h>
#include <conio.h>

typedef struct _nod
{
   char c;
   _nod *s, *d, *t;
} nod;

char s[101];
int pos= 0;

nod* CreareArbore(nod *q)
{
   nod *n;
   int a;
   n= new nod;
   n->c= s[pos];
   pos+= 2;
   a= s[pos]-'0';
   n->s= NULL;
   n->d= NULL;
   n->t= q;
   if (a == 2)
   {
      q= n;
      pos+= 2;
      n->s= CreareArbore(q);
      pos+= 2;
      n->d= CreareArbore(q);
   }
   if (a == 1)
   {
      q= n;
      pos+= 2;
      n->s= CreareArbore(q);
   }

   return n;
}

Afisare(nod *n)
{
   if (n != NULL)
   {
      printf(" %c", n->c);
      Afisare(n->s);
      Afisare(n->d);
   }

   return 0;
}

main()
{
   nod *n, *p, *q;

   clrscr();
   printf("\n");

   printf(" Arborele afisat in preordine (+ dupa fiecare nod nr. de fii) : \n");
   printf(" ");
   gets(s);
   n= CreareArbore(NULL);
   Afisare(n);
   for (p= n; p->s != NULL; p= p->s);
   printf("%c... ", p->c);
   q= p;
   for (p= q; p->t != NULL; p= p->t);
   printf("%c !!!", p->c);

   getch();
   return 0;
}