#include <stdio.h>
#include <conio.h>
#include "coada.h"

Coada::Coada(int x)
{
   t= new int[x];
   u= 0;
}

Coada::~Coada()
{
   delete [] t;
}

void Coada::ad(int x)
{
   for (int i= u; i > 0; i--)
      t[i]= t[i-1];
   t[0]= x;
   u++;
}

void Coada::sc()
{
   u--;
}

void Coada::lst()
{
   printf("\n Coada : ");
   for (int i= 0; i < u; i++)
      printf("%i ", t[i]);
   getch();
}

void Coada::caut(int x)
{
   int aux= 0;

   for (int i= 0; i < u; i++)
      if (t[i] == x)
      {
	 aux= 1;
	 i= u;
      }
   (aux == 1)?printf("\n Elementul exista in coada."):printf("\n Elementul nu exista in coada.");
   getch();
}