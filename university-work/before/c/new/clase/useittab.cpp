#include <iostream.h>
#include <conio.h>
#include "tpl_tab.h"
#include "it_tab.h"

void iterare(int& x, void *arg)
{
   int nr= *((int*)arg);
   x+= nr;
}

int MaiMare(const int& x, void *arg)
{
   int nr= *((int*)arg);

   if (x > nr)
      return 1;
   return 0;
}

main()
{
   clrscr();

   Tablou<int> t;
   TablouIterator<int> tit(t);
   int n;

   t.Adaug(1);
   t.Adaug(2);
   t.Adaug(5);
   t.Adaug(4);
   t.Adaug(3);

   while (tit)
   {
      n= tit();
      tit++;
      cout<<n<<' ';
   }
   cout<<"\n";

//   int a= 2;
//   t.ForEach(iterare, &a);  // De ce dam "&a"?
//   tit.Restart();

   int a= 3;
   const int *b= t.LastThat(MaiMare, &a);
   cout<<*b;

//   while (tit)
//   {
//      n= tit();
//      tit++;
//      cout<<n<<' ';
//   }

   getch();
   return 0;
}