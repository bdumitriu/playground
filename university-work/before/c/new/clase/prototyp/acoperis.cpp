#include "acoperis.h"
#include <stdio.h>

Acoperis::Acoperis(int cul, int ah)
{
   Culoare = cul;
   AreHorn = ah;
}

void Acoperis::Afisare() const
{
   if (AreHorn)
      printf("Acoperis de culoare %d, cu horn.", Culoare);
   else
      printf("Acoperis de culoare %d, fara horn.", Culoare);
}

Acoperis* Acoperis::Clone() const
{
   return new Acoperis(Culoare, AreHorn);
}