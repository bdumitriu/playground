#include <stdio.h>
#include <conio.h>
#include "ent_gr.h"
#include "imagine.h"
#include "punct.h"
#include "linie.h"
#include "cerc.h"
#include "dreptung.h"

main()
{
   Imagine img1;
   Punct p1(10, 10);
   Linie l1(1, 1, 10, 20);
   Linie l2(2, 6, 2, 7);
   Imagine img2;
   Dreptunghi d1(10, 1, 1, 10);
   Cerc c1(1, 1, 5);
   Cerc c2(4, 2, 10);

   clrscr();

   img1.Adaug(&p1);
   img1.Adaug(&l1);
   img1.Adaug(&l2);
   img1.Adaug(&img2);
   img2.Adaug(&c1);

   img1.Afisare();

   return 0;
}