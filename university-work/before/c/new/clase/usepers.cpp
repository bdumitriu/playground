#include "pers.h"
#include "tpl_tab.h"
#include <fstream.h>

main()
{
   Pers p("Dumitriu", "Bogdan");
   Tablou<Pers> a(5);
   Tablou<int> b(6);

   a.Adaug(p);
   b.Adaug(5);
   a.Adaug(Pers("Becker", "Boris"));
   b.Adaug(10);
//   a.List();
//   b.List();
   cout<<"\n\n";
   a.List(cout);
   b.List(cout);

   fstream fOut("bogdan.txt",ios::out);
//   fOut<<a<<"\n"<<b;
   a.List(fOut);
   b.List(fOut);

   return 0;
}


