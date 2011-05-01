#include "imagine.h"
#include "punct.h"
#include "cerc.h"
#include "linie.h"

main()
{
   Imagine c;
   Imagine d;
   Cerc cer1(1,2,2);
   Linie lin(1,1,10,10);
   Cerc cer2(2,4,6);
   Cerc cer3(1,6,5);

   c.Adaug(&cer2);
   c.Adaug(&cer1);
   c.Adaug(&lin);
   c.Adaug(&cer3);

   d.Adaug(&cer1);
   c.Adaug (&d);

   c.Afisare();

   return 0;
}