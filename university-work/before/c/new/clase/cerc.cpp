#include <iostream.h>
#include "punct.h"
#include "cerc.h"
#include <math.h>
#define PI 3.141592653589793

Cerc::Cerc(int x, int y, int raza)
   : EntitateGrafica(x, y)
{
   r = raza;
}

Cerc::~Cerc()
{}

void Cerc::Afisare() const
{
   cout<<"Cercul de centru ("<<x<<','<<y<<") si de raza "<<r;
}

double Cerc::Arie() const
{
   return PI*r*r;
}

double Cerc::Perimetru() const
{
   return 2*PI*r;
}

int Cerc::Raza() const
{
   return r;
}

int Cerc::Cx() const
{
   return x;
}

int Cerc::Cy() const
{
   return y;
}

Cerc& Cerc::operator=(const Cerc& c)
{
   if (*this != c)
   {
      x = c.Cx();
      y = c.Cy();
      this->r = c.Raza();
   }

   return *this;
}

int Cerc::operator==(const EntitateGrafica& c) const
{
   return ((x == ((Cerc&)c).Cx()) &&
	   (y == ((Cerc&)c).Cy()) &&
	   (r == ((Cerc&)c).Raza()));
}

int Cerc::operator!=(const Cerc& c) const
{
   return !(*this == c);
}

ostream& operator<<(ostream& os, const Cerc& c)
{
   os<<c.Cx()<<' '<<c.Cy()<<' '<<c.Raza();

   return os;
}

istream& operator>>(istream& is, Cerc& c)
{
   int x, y, z;

   is>>x;
   is.get();
   is>>y;
   is.get();
   is>>z;

   Cerc cer(x, y, z);
   c = cer;

   return is;
}