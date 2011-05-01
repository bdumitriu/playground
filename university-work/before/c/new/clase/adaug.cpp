#include "tpl_tab.h"

main()
{
   Tablou<int> t(2);

   clrscr();

   t.Adaug(10);
   t.Adaug(12);
   t.Adaug(15);
   t.List();

   return 0;
}