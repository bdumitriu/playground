#include "punct.h"
#include <iostream.h>
#include <stdio.h>

Punct::Punct()
{}

Punct::Punct(int x, int y)
   : EntitateGrafica(x, y)
{}

Punct::~Punct()
{}

void Punct::Afisare() const
{
   cout<<"Punctul: "<<x<<","<<y;
}

double Punct::Perimetru() const
{
   return 0;
}

double Punct::Arie() const
{
   return 0;
}

int Punct::operator==(const EntitateGrafica& p) const
{
   return ((x == p.X()) && (y == p.Y()));
}

int Punct::operator!=(const Punct& p) const
{
   return !(*this == p);
}

Punct& Punct::operator=(const Punct& p)
{
   if (p != *this)
   {
      x = p.x;
      y = p.y;
   }

   return *this;
}

ostream& operator<<(ostream& os, const Punct& p)
{
   os<<p.x<<" "<<p.y;

   return os;
}

istream& operator>>(istream& is, Punct& p)
{
   int a, b;

   is>>a;
   is.get();
   is>>b;

   Punct p_aux(a, b);

   p = p_aux;

   return is;
}