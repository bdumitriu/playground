#include "stivat.h"
#include <conio.h>

main()
{
   Stivat s;

   clrscr();

   s.ad(10);
   s.sc();
   s.ad(20);
   s.ad(14);
   s.ad(31);
   s.sc();
   s.lst();
   s.caut(20);
   s.caut(31);
   s.ad(54);
   s.lst();

   return 0;
}
