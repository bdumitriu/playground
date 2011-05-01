#include "tpl_tab.h"
#include "tab_sort.h"
#include <iostream.h>
#include <fstream.h>
#include <conio.h>

int compar(const int& x1, const int& x2)
{
   return (x1>x2);
}

main()
{

  TablouSortat<int> ts1(compar,10);

  clrscr();

  ts1.Adaug(2);
  ts1.Adaug(12);
  ts1.Adaug(22);
  ts1.Adaug(3);
  ts1.Adaug(112);
  ts1.List();

/*
   Tablou<int> t1(100), t3(100);
   Tablou<char> t2;

   clrscr();

   t1.Adaug(34);
   t2.Adaug('h');
   t1.Adaug(12);
   t2.Adaug('a');
   t1.Adaug(10);
   t2.Adaug('w');
   t1.List();
   t2.List();

   ofstream f("pt_tabl");

   f<<t1;
   f.close();

   ifstream g("pt_tabl");
   g>>t3;
   t3.List();
   t3.Adaug(123);

   if (t1 != t3)
      cout<<"Nu";
   else
      cout<<"Da";

   getch();
*/
   return 0;
}