// Exemplu de sir initial "a 2 1 1 b 1 0 1 d 0 c 0" pentru arborele
//                         a
//                        / \
//                       b   c
//                        \
//                         d
#include <stdio.h>
#include <conio.h>

struct nod
{
   char c;
   nod *s, *d;
};

char s[101];
int pos= 0;

nod* CreareArbore()
{
   int a, b, c;
   nod *n;

   n= new nod;
   n->c= s[pos];
   pos+= 2;
   a= s[pos]-'0';
   n->s= NULL;
   n->d= NULL;
   if ((a == 1) || (a == 2))
   {
      pos+= 2;
      b= s[pos]-'0';
      pos+= 2;
      c= s[pos]-'0';
      if (b == 1)
      {
	 pos+= 2;
	 n->s= CreareArbore();
      }
      if (c == 1)
      {
	 pos+= 2;
	 n->d= CreareArbore();
      }
   }

   return n;
}

Afisare(nod *n)
{
   if (n != NULL)
   {
      printf("%c ", n->c);
      Afisare(n->s);
      Afisare(n->d);
   }

   return 0;
}

main()
{
   nod *n;

   clrscr();
   printf("\n");

   printf(" Sirul de caractere : ");
   gets(s);
   printf(" ");
   n= CreareArbore();
   Afisare(n);

   return 0;
}