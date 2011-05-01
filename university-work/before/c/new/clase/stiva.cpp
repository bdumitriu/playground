#include <stdio.h>
#include <conio.h>
#include "stiva.h"

Stiva::Stiva()
{
}

Stiva::ad(int x)
{
   Stiva *p;

   p= (Stiva *) malloc(sizeof(Stiva));
   p->leg= varf;
   p->n= x;
   varf= p;
}

Stiva::sc()
{
   Stiva *p;

   p= varf;
   varf= varf->leg;
   delete p;
}

Stiva::lst()
{
   Stiva *p;

   p= varf;
   printf("\n Stiva : ");
   while (p != NULL)
   {
      printf("%i ", p->n);
      p= p->leg;
   }
   getch();
}

Stiva::caut(int x)
{
   Stiva *p;

   p= varf;
   int aux= 0;
   while (p != NULL)
   {
      if (p->n == x)
	 aux= 1;
      p= p->leg;
   }
   (aux == 1)?printf("\n Elementul exista in stiva."):printf("\n Elementul nu exista in stiva.");
}

Stiva::~Stiva()
{
   Stiva *p, *q;

   p= varf;
   while (p != NULL)
   {
      q= p;
      p= p->leg;
      delete q;
   }
}