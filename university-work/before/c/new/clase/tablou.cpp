#include <stdio.h>
#include <conio.h>
#include "tablou.h"

Tablou::Tablou(int d)
{
   tab= new int[d];
   dim= d;
   ind= 0;
}

Tablou::~Tablou()
{
   delete [] tab;
}

void Tablou::adaug(int x)
{
   tab[ind++]= x;
}

void Tablou::scot(int x)
{
   for (int i= 0; i < ind; i++)
      if (tab[i] == x)
      {
	 for (int j= i; j < ind-1; j++)
	    tab[j]= tab[j+1];
	 ind--;
	 break;
      }
}

void Tablou::list()
{
   printf("\nTabloul : ");
   for (int i= 0; i < ind; i++)
      printf("%i ", tab[i]);
   getch();
}

int& Tablou::operator[](int idx)
{
   return tab[idx];
}