#include <iostream.h>
#include "cerc.h"
#include "punct.h"
#include <conio.h>

main()
{
/*
   Cerc c(5, 7, 7);
   Punct p(12, 7);

   if (c.Pin(p))
      cout<<"BINE";
   else
      cout<<"RAU";
*/
   Punct p(10, 10);
   Cerc c1(p, 5), c2(20, 20, 10);

   clrscr();

   cout<<c1;
   cout<<'\n';
   c1.List();
   cout<<'\n';
   cout<<c1.Arie();
   cout<<'\n';
   cout<<c1.Lung();
   cout<<'\n';
   if (c1 == c2)
      cout<<"c1 = c2";
   else
      cout<<"c1 != c2";
   cout<<'\n';
   c1= c2;
   if (c1 == c2)
      cout<<"c1 = c2";
   else
      cout<<"c1 != c2";

   getch();

   return 0;
}