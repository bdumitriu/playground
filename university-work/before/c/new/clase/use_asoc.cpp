#include <stdio.h>
#include <iostream.h>
#include <fstream.h>
#include "tpl_asoc.h"
#include "str.h"

main()
{
   int i;
   Asoc<String, String> el1("printf", "stdio"), el5;
   Asoc<String, int> el2("10", 20), el6;
   Asoc<int, String> el3(34, "orice vrei"), el7;
   Asoc<int, int> el4(103, 345), el8;

   ofstream f("xxx.tst");

//   String s;
//   Asoc<String, String> a[10];

   clrscr();

   f<<el2;//<<el2<<el3<<el4;

   ifstream g("xxx.tst");

   g>>el6;//>>el6>>el7>>el8;

//   el5.List(cout);
   el6.List(cout);
//   el7.List(cout);
//   el8.List(cout);

   getch();
/*
   i= 1;
   if (i == 1)
   {
      Asoc<String, String> a0("clrscr", "conio");
      a[0]= a0;
   }
   if (i == 1)
   {
      Asoc<String, String> a0("scanf", "stdio");
      a[1]= a0;
   }
   if (i == 1)
   {
      Asoc<String, String> a0("getch", "conio");
      a[2]= a0;
   }

   a[0].List();
   cout<<"\n";
   a[1].List();
   cout<<"\n";
   a[2].List();
   cout<<"\n";

   el.List();
   cout<<"\n";
   s= el.Cheie();
   s.List();
   getch();
*/

   return 0;
}