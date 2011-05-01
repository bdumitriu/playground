#include <stdio.h>
#include <conio.h>
#include "tablou.h"

void main()
{
   Tablou t1;
   Tablou t2(10);

   t1.adaug(200);
   t2.adaug(2000);
   t1.adaug(300);
   t2.adaug(3000);
   t1.adaug(400);
   t2.adaug(4000);
   t1.list();
   t2.list();

   printf("\nElem 1 din t1: %d",t1[1]);
   getch();

   t1[1]=333;
   t1.list();

}

