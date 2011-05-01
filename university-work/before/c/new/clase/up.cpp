#include <conio.h>
#include <iostream.h>
#include "ent_gr.h"
#include "punct.h"
#include "cerc.h"
#include "linie.h"
#include "dreptung.h"
#include "imagine.h"

main()
{
   Cerc c(10, 20 ,5);
   Linie l(1, 1, 10, 100);
   EntitateGrafica *a;
   Imagine i;

   clrscr();

   i.Adaug(&c);
   i.Adaug(&l);
   i.Afisare();
   if (i.Contine(c))
      cout<<"Da";


   getch();
   return 0;
}