#include "acopct.h"
#include <stdio.h>

Acoperisct::Acoperisct(int nr, int cul, int ah)
   : Acoperis(cul, ah)
{
   nr_tig = nr;
}

void Acoperisct::Afisare() const
{
   Acoperis::Afisare();
   printf(" Acoperisul are %d tigle.", nr_tig);
}

Acoperis* Acoperisct::Clone() const
{
   return new Acoperisct(nr_tig, Culoare, AreHorn);
}