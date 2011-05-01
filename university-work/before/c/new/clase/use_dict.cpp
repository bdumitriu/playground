#include <fstream.h>
#include "tpl_dict.h"
#include "str.h"

main()
{
   Dict<String, String> d(10), e(3);

   d.Adauga("1", "unu");
   d.Adauga("32", "treisdoi");

   ofstream f("fistest.dic");

   f<<d;

   ifstream g("fistest.dic");

   g>>e;
   e.List();

   getch();
   return 0; 
}