#include "percar.h"
#include <stdio.h>

Percar::Percar(int nr, int cul)
   : Perete(cul)
{
   nr_car = nr;
}

void Percar::Afisare() const
{
   Perete::Afisare();
   printf(" Peretele are %d caramizi.", nr_car);
}

Perete* Percar::Clone() const
{
   return new Percar(nr_car, Culoare);
}