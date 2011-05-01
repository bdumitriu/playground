#include "punct.h"
#include <conio.h>
#include <iostream.h>

main()
{
   Punct p1(2, 3);
   Punct p2(2, 4);

   clrscr();
   cout<<"Punctul p1: ";
   p1.List();
   cout<<"\n";
   cout<<"Punctul p2: ";
   p2.List();
   cout<<"\n";
   getch();
   if (p1 == p2)
      cout<<"p1 egal cu p2";
   else
      cout<<"p1 diferit de p2";
   cout<<"\n";
   getch();
   if (p1 != p2)
      cout<<"p1 diferit de p2";
   else
      cout<<"p1 egal cu p2";
   cout<<"\n";
   getch();
   p1= p2;
   p1.List();
   cout<<"\n";
   getch();
   p2= p1;
   p2.List();
   cout<<"\n";
   getch();

   return 0;
}