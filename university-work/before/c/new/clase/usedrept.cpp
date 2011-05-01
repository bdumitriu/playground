#include "dreptung.h"
#include "punct.h"
#include <conio.h>

main()
{
   Punct x1(10, 10), x2(100, 200);
   Dreptunghi d1(x1, x2), d2(10, 20, 14, 50);

   clrscr();

   d1.List();
   getch();
   cout<<'\n';
   d2.List();
   getch();
   cout<<'\n';
   if (d1 == d2)
      cout<<"d1 = d2";
   else
      cout<<"d1 != d2";
   getch();
   cout<<'\n';
   cout<<d1.Lung_lat1()<<' '<<d1.Lung_lat2();
   getch();
   cout<<'\n';
   cout<<d2.Lung_lat1()<<' '<<d2.Lung_lat2();
   getch();
   cout<<'\n';
   cout<<"Perimetrul d1: "<<d1.Perimetru();
   getch();
   cout<<'\n';
   cout<<"Perimetrul d2: "<<d2.Perimetru();
   getch();
   cout<<'\n';
   cout<<"d1, colt stanga sus : "<<d1.Colt_st_sus();
   getch();
   cout<<'\n';
   cout<<"d1, colt dreapta jos : "<<d1.Colt_dr_jos();
   getch();
   cout<<'\n';

   return 0;
}
