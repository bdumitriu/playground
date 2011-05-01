#include "punct.h"
#include "cerc.h"
#include "dreptung.h"
#include <conio.h>
#include <iostream.h>

main()
{
   Cerc *c[50];
   Dreptunghi *d[50];
   int n_c, n_d, i, x, y, r;

   clrscr();

   cout<<"\n Numarul de cercuri : ";
   cin>>n_c;
   i= 0;
   while (i < n_c)
   {
      i++;
      cout<<" Coordonatele centrului cercului "<<i<<" : ";
      cin>>x>>y;
      cout<<" Raza cercului "<<i<<" : ";
      cin>>r;

      Cerc cer(x, y, r);

      c[i]= &cer;
   }

   (*c[0]).List();

   return 0;
}