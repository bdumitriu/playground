#include "linie.h"
#include <iostream.h>
#include <math.h>

Linie::Linie()
{}

Linie::Linie(int x1, int y1, int x2, int y2)
   : EntitateGrafica(x1, y1)
{
   this->x1 = x2;
   this->y1 = y2;
}

Linie::~Linie()
{}

void Linie::Afisare() const
{
   cout<<"Linia de la ("<<x<<','<<y<<") la ("<<x1<<','<<y1<<')';
}

double Linie::Perimetru() const
{
   return (sqrt((x1-x)*(x1-x)+(y1-y)*(y1-y)));
}

double Linie::Arie() const
{
   return Perimetru();
}

int Linie::X1() const
{
   return x;
}

int Linie::Y1() const
{
   return y;
}

int Linie::X2() const
{
   return x1;
}

int Linie::Y2() const
{
   return y1;
}

Linie& Linie::operator=(const Linie& l)
{
   if (*this != l)
   {
      x = l.X1();
      y = l.Y1();
      x1 = l.X2();
      y1 = l.Y2();
   }

   return *this;
}

int Linie::operator==(const EntitateGrafica& l) const
{
   return ((x == ((Linie&)l).X1()) &&
	   (y == ((Linie&)l).Y1()) &&
	   (x1 == ((Linie&)l).X2()) &&
	   (y1 == ((Linie&)l).Y2()));
}

int Linie::operator!=(const Linie& l) const
{
   return !(*this == l);
}

ostream& operator<<(ostream& os, const Linie& l)
{
   os<<l.x<<' '<<l.y<<' '<<l.x1<<' '<<l.y1;

   return os;
}

istream& operator>>(istream& is, Linie& l)
{
   int x1, y1, x2, y2;

   is>>x1;
   is.get();
   is>>y1;
   is.get();
   is>>x2;
   is.get();
   is>>y2;

   Linie x(x1, y1, x2, y2);

   l = x;

   return is;
}