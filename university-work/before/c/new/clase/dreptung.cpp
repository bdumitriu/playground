#include "dreptung.h"
#include <conio.h>
#include <math.h>

Dreptunghi::Dreptunghi(int x1, int y1, int x2, int y2)
   : EntitateGrafica(x1, y1)
{
   this->x1 = x2;
   this->y1 = y2;
}

Dreptunghi::~Dreptunghi()
{}

void Dreptunghi::Afisare() const
{
   cout<<"Dreptunghiul cu colturile de coordonate ("<<x<<','<<y<<") si ("<<x1<<','<<y1;
}

int Dreptunghi::Lung_lat1() const
{
   return abs(x-x1);
}

int Dreptunghi::Lung_lat2() const
{
   return abs(y-y1);
}

double Dreptunghi::Perimetru() const
{
   return 2*this->Lung_lat1()+2*this->Lung_lat2();
}

double Dreptunghi::Arie() const
{
   return this->Lung_lat1()*this->Lung_lat2();
}

int Dreptunghi::Sx() const
{
   return x;
}

int Dreptunghi::Sy() const
{
   return y;
}

int Dreptunghi::Dx() const
{
   return x1;
}

int Dreptunghi::Dy() const
{
   return y1;
}

Dreptunghi& Dreptunghi::operator=(const Dreptunghi& d)
{
   if (*this != d)
   {
      x = d.Sx();
      y = d.Sy();
      x1 = d.Dx();
      y1 = d.Dy();
   }

   return *this;
}

int Dreptunghi::operator==(const EntitateGrafica& d) const
{
   return ((x == ((Dreptunghi&)d).Sx()) &&
	   (y == ((Dreptunghi&)d).Sy()) &&
	   (x1 == ((Dreptunghi&)d).Dx()) &&
	   (y1 == ((Dreptunghi&)d).Dy()));
}

int Dreptunghi::operator!=(const Dreptunghi& d) const
{
   return !(*this == d);
}

ostream& operator<<(ostream& os, const Dreptunghi& d)
{
   os<<d.Sx()<<' '<<d.Sy()<<' '<<d.Dx()<<' '<<d.Dy();

   return os;
}

istream& operator>>(istream& is, Dreptunghi& d)
{
   int x1, y1, x2, y2;

   is>>x1;
   is.get();
   is>>y1;
   is.get();
   is>>x2;
   is.get();
   is>>y2;

   Dreptunghi x(x1, y1, x2, y2);

   d = x;

   return is;
}