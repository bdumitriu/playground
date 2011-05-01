#include "linie.h"
#include "punct.h"
#include <conio.h>
#include <iostream.h>

main()
{
   Punct p1(1, 1), p2(10, 20), p3(5, 5), p4(100, 130);
   Linie l1(p1, p2), l2(p3, p4), l3(1, 2, 3, 4);

   clrscr();

   l1.List();
   cout<<"\n";
   l2.List();
   cout<<"\n";
   l3.List();
   cout<<"\n";
   getch();

   if (l1 == l2)
      cout<<"l1 = l2";
   else
      cout<<"l1 != l2";
   getch();
   cout<<"\n";
   l1= l2;
   if (l1 == l2)
      cout<<"l1 = l2";
   else
      cout<<"l1 != l2";
   getch();
   cout<<"\n";
   cout<<l1;
   cout<<"\n";
   getch();

   return 0;
}