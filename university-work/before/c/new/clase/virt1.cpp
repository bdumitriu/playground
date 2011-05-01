#include "tab_sort.h"

int Sort(const int& x, const int& y)
{
   return x<y;
}

void Adauga10(Tablou<int>& t)
{
   t.Adaug(8);
   t.Adaug(2);
   t.Adaug(4);
   t.Adaug(1);
   t.Adaug(5);
   t.Adaug(9);
   t.Adaug(6);
}

void main()
{
   Tablou<int> t1(10);
   TablouSortat<int> t2(Sort,10);

   Adauga10(t1);
   Adauga10(t2);

   t1.List();
   t2.List();
}
