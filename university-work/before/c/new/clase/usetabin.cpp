#include "tpl_tabi.h"

void iterare(int& x, void *arg)
{
   int nr= *((int*)arg);
   x+= nr;
}


main()
{
   TablouI<int> t(10, 3, 1);


   t.Adaug(new int(5));
   t.Adaug(new int(3));
   t.Adaug(new int(7));
   t.Adaug(new int(1));
   t.List();

   int x=2;
   t.ForEach(iterare,&x);
   t.List();

   return 0;
}