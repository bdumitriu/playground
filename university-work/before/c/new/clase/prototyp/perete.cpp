#include "perete.h"
#include <stdio.h>

Perete::Perete(int cul)
{
   Culoare = cul;
}

void Perete::Afisare() const
{
      printf("Perete de culoare %d.", Culoare);
}

Perete* Perete::Clone() const
{
   return new Perete(Culoare);
}