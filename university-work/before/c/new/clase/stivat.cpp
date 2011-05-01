#include <stdio.h>
#include <conio.h>
#include "stivat.h"

Stivat::Stivat(int x)
{
   t= new int[x];
   varf= 0;
}

Stivat::~Stivat()
{
   delete [] t;
}

void Stivat::ad(int x)
{
   t[varf++]= x;
}

void Stivat::sc()
{
   varf--;
}

void Stivat::lst()
{
   printf("\n Stiva : ");
   for (int i= 0; i < varf; i++)
      printf("%i ", t[i]);
   getch();
}

void Stivat::caut(int x)
{
   int aux= 0;

   for (int i= 0; i < varf; i++)
      if (x == t[i])
      {
	 aux= 1;
	 i= varf;
      }
   (aux == 1)?printf("\n Elementul exista in stiva."):printf("\n Elementul nu exista in stiva.");
   getch();
}