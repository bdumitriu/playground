#include <conio.h>
#include <stdio.h>
#include <alloc.h>
struct elem
       {
	  struct elem *urm;
	  int i;
       };
struct elem *cap, *p, *q;

creare()
{
   int i, x;

   printf("\n");
   cap= (struct elem*) malloc(sizeof(struct elem));
   cap->urm= NULL;
   q= cap;
   i= 1;
   printf(" Elementul %i : ", i);
   scanf("%i", &x);
   while (x != 0)
   {
      p= (struct elem*) malloc(sizeof(struct elem));
      p->i= x;
      q->urm= p;
      p->urm= NULL;
      q= p;
      i= i+1;
      printf(" Elementul %i : ", i);
      scanf("%i", &x);
   }

   printf("\n Lista a fost creata.");
   getch();
   return 0;
}

stergere_elem()
{
   int x, test= 0;
   char i;

   printf("\n");
   if (cap->urm == NULL)
      printf(" Lista e goala.");
   else
   {
      printf(" Introduceti numarul pe care doriti sa-l stergeti : ");
      scanf("%d", &x);
      printf(" Doriti sa stergeti toate elementele ce contin ca informatie\n");
      printf(" acel numar sau doar primul (t/p) ? ");
      getch();
      scanf("%c", &i);
     // i= 't';
      if (i == 't')
      {
	 p= cap->urm;
	 q= cap;
	 while (p != NULL)
	 {
	    if (p->i == x)
	    {
	       q->urm= p->urm;
	       free(p);
	       test= 1;
	       p= q->urm;
	    }
	    else
	    {
	       q= q->urm;
	       p= p->urm;
	    }
	 }
      }
   if (!test)
      printf("\n Elementul nu a fost gasit in lista.\n");
   else printf("\n Elementul a fost sters.\n");
   }

   getch();
   return 0;
}

stergere_lista()
{
   p= cap->urm;
   if (p == NULL)
      printf("\n Lista este deja goala.\n");
   else
   {
      while (p != NULL)
      {
	 cap->urm= p->urm;
	 free(p);
	 p= cap->urm;
      }

      printf("\n Lista a fost stearsa.\n");
   }

   getch();
   return 0;
}

adaugare()
{
   int x;

   p= cap->urm;
   while (p->urm != NULL)
      p= p->urm;
   q= (struct elem*) malloc(sizeof(struct elem));
   p->urm= q;
   q->urm= NULL;
   printf("\n");
   printf(" Elementul : ");
   scanf("%i", &x);
   q->i= x;

   printf("\n Elementul a fost adaugat. \n");
   getch();
   return 0;
}

test()
   {
      p= cap->urm->urm;
      q= cap->urm;
      while (p != NULL)
	 if (p->i < q->i)
	    return 1;
	 else
	 {
	    q= p;
	    p= p->urm;
	 }
      return 0;
   }

ordonare()
{

   int aux;

   do
   {
      p= cap->urm->urm;
      q= cap->urm;
      while (p != NULL)
      {
	 if (q->i > p->i)
	 {
	    aux= q->i;
	    q->i= p->i;
	    p->i= aux;
	 }
	 q= p;
	 p= p->urm;
      }
   }
   while (test() == 1);

   printf("\n Lista a fost ordonata.\n");
   getch();
   return 0;
}

listare()
{
   printf("\n");
   p= cap->urm;
   if (p == NULL)
      printf(" Lista e goala.");
   else
   {
      printf(" Lista este :");
      while (p != NULL)
      {
	 printf(" %i", p->i);
	 p= p->urm;
      }
   }

   getch();
   printf("\n");

   return 0;
}

main()
{
   char c;

   do
   {
      clrscr();
      printf("\n");
      printf("Aveti la dispozitie urmatoarele optiunui : \n");
      printf(" c - creare lista\n");
      printf(" l - listare lista\n");
      printf(" s - stergerea unui element din lista\n");
      printf(" t - stergerea intregii liste\n");
      printf(" a - adaugarea unui element in lista\n");
      printf(" o - ordonarea listei in ordine crescatoare\n");
      printf(" e - iesire din program\n");
      printf("\n");
      printf(" Optiunea dvs. : ");
      scanf("%c", &c);
      switch (c)
      {
	 case 'c' : creare(); break;
	 case 'l' : listare(); break;
	 case 's' : stergere_elem(); break;
	 case 't' : stergere_lista(); break;
	 case 'a' : adaugare(); break;
	 case 'o' : ordonare(); break;
      }
   }
   while (c != 'e');

   return 0;
}
