#include <stdio.h>
#include <conio.h>
#include "pers.h"

Pers::Pers()
{ }

Pers::Pers(const String& n, const String& p)
  :nume(n), prenume(p)
{}

Pers::~Pers()
{}

Pers::Pers(const Pers& p)
{
   nume= p.nume;
   prenume= p.prenume;
}

Pers& Pers::operator=(const Pers& p)
{
   if (this != &p)
   {
      nume= p.nume;
      prenume= p.prenume;
   }
   return *this;
}

int Pers::operator==(const Pers& p) const
{
   return ((nume == p.nume) && (prenume == p.prenume));
}

ostream& operator<<(ostream& os, const Pers& p)
{
   os<<p.nume<<' '<<p.prenume;
   return os;
}

void Pers::List() const
{
   nume.List();
   prenume.List();
}