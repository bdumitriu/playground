#include "casa.h"
#include <stdio.h>

Casa::Casa(const Acoperis& a, const Perete& p)
{
   ac = a.Clone();
   per = p.Clone();
}

Casa::~Casa()
{
   delete ac;
   delete per;
}

void Casa::Afisare() const
{
   printf("Casa are urmatoarele proprietati : \n");
   ac->Afisare();
   printf("\n");
   per->Afisare();
   printf("\n");
}