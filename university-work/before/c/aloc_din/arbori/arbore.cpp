#include <stdio.h>
#include <conio.h>
#include <alloc.h>
struct arb
       {
	  int i;
	  struct arb *s, *d;
       };
struct arb *rad, *p, *q;

creare()
{
   int i, j, test;

   rad= (struct arb*) malloc(sizeof(struct arb));
   rad->s= NULL;
   rad->d= NULL;
   p= rad;
   q= rad;
   j= 1;
   printf("Elementul %i : ", j);
   scanf("%i", &i);
   while (i != 0)
   {
      if (j == 1)
	    rad->i= i;
      else
      {
	 p= rad;
	 q= rad;
	 while (p != NULL)
	    if (i < p->i)
	    {
	       q= p;
	       p= p->s;
	       test= 1;
	    }
	    else
	    {
	       q= p;
	       p= p->d;
	       test= 2;
	    }
	 if (test == 1)
	 {
	    p= (struct arb*) malloc(sizeof(struct arb));
	    p->i= i;
	    p->s= NULL;
	    p->d= NULL;
	    q->s= p;
	 }
	 else
	 {
	    p = (struct arb*) malloc(sizeof(struct arb));
	    p->i= i;
	    p->s= NULL;
	    p->d= NULL;
	    q->d= p;
	 }
      }
      j= j+1;
      printf("Elementul %i : ", j);
      scanf("%i", &i);
   }

   return 0;
}

nod_t(int test)
{
   if (p == rad)
      rad= NULL;
   else
   {
      if (test == 1)
	 q->s= NULL;
      if (test == 2)
	 q->d= NULL;
      free(p);
   }
   return 0;
}

fiu_s(int test)
{
   if (p == rad)
      rad= rad->s;
   else
   {
      if (test == 1)
	 q->s= p->s;
      if (test == 2)
	 q->d= p->s;
   }
   free(p);

   return 0;
}

fiu_d(int test)
{
   if (p == rad)
      rad= rad->d;
   else
   {
      if (test == 1)
	 q->s= p->d;
      if (test == 2)
	 q->d= p->d;
   }
   free(p);

   return 0;
}

doi_fii()
{
   struct arb *n;

   n= p;
   p= p->d;
   while (p->s != NULL)
   {
      q= p;
      p= p->s;
   }
   n->i= p->i;
   if (n->d == p)
      n->d= p->d;
   else
      q->s= p->d;
   free(p);

   return 0;
}

stergere()
{
   int i, test;
   printf("Elementul pe care doriti sa-l stergeti : ");
   scanf("%i", &i);
   p= rad;
   q= rad;
   while (p->i != i)
   {
      if (i < p->i)
      {
	 q= p;
	 p= p->s;
	 test= 1;
      }
      else
      {
	 q= p;
	 p= p->d;
	 test= 2;
      }
   }
   if ((p->s == NULL) && (p->d == NULL)) nod_t(test);
   else if ((p->s == NULL) && (p->d != NULL)) fiu_d(test);
   else if ((p->s != NULL) && (p->d == NULL)) fiu_s(test);
   else if ((p->s != NULL) && (p->d != NULL)) doi_fii();

   return 0;
}

adaugare()
{
   int i, test;

   p= rad;
   q= rad;
   printf("Elementul : ");
   scanf("%i", &i);
   if (rad == NULL)
   {
      rad= (struct arb*) malloc(sizeof(struct arb));
      rad->s= NULL;
      rad->d= NULL;
      rad->i= i;
   }
   else
   {
      p= rad;
      q= rad;
      while (p != NULL)
      {
	 if (i < p->i)
	 {
	    q= p;
	    p= p->s;
	    test= 1;
	 }
	 else
	 {
	    q=p;
	    p= p->d;
	    test= 2;
	 }
      }
      if (test == 1)
      {
	 p= (struct arb*) malloc(sizeof(struct arb));
	 p->i= i;
	 p->s= NULL;
	 p->d= NULL;
	 q->s= p;
      }
      else
      {
	 p = (struct arb*) malloc(sizeof(struct arb));
	 p->i= i;
	 p->s= NULL;
	 p->d= NULL;
	 q->d= p;
      }
   }

   return 0;
}

listare_cr(struct arb *p)
{
   if (p != NULL)
   {
      listare_cr(p->s);
      printf(" %i", p->i);
      listare_cr(p->d);
   }

   return 0;
}

main()
{
   char c;

   do
   {
      clrscr();
      printf("\n");
      printf("Aveti la dispozitie urmatoarele comenzi : \n");
      printf(" c - crearea arborelui.\n");
      printf(" l - listarea arborelui in ordine crescatoare.\n");
      printf(" s - stergerea unui nod.\n");
      printf(" a - adaugarea unui nod.\n");
      printf(" e - sfarsitul programului.\n\n");
      printf(" Comanda dvs. : ");
      scanf("%c", &c);
      printf("\n");
      switch (c)
      {
	 case 'c' :
	 {
	    creare();
	    printf("Arborele a fost creat.");
	    getch();
	    break;
	 }
	 case 'l' :
	 {
	    if (rad == NULL)
	       printf("Arborele e gol.");
	    else
	    {
	       listare_cr(rad);
	       getch();
	       break;
	    }
	 }
	 case 's' :
	 {
	    if (rad == NULL)
	       printf("Arborele e gol.");
	    else
	    {
	       stergere();
	       printf("Nodul a fost sters.");
	       getch();
	       break;
	    }
	 }
	 case 'a' :
	 {
	    adaugare();
	    printf("Nodul a fost adaugat.");
	    getch();
	    break;
	 }
      }
   }
   while (c != 'e');

   return 0;
}