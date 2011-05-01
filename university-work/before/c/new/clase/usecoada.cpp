#include <conio.h>
#include "coada.h"

main()
{
   Coada c(20);

   clrscr();

   c.ad(10);
   c.ad(10);
   c.sc();
   c.ad(20);
   c.ad(25);
   c.lst();
   c.caut(10);
   c.caut(100);
}