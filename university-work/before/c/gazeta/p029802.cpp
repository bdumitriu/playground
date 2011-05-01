// P029802   Bogdan Dumitriu, Liceul Teoretic Gh. Sincai, cls. XI-E,
//           Cluj-Napoca

#include <conio.h>
#include <iostream.h>
#include <fstream.h>
#include <string.h>
#include <stdlib.h>

ofstream fOut("intermed.aux");

void CitireDinFisier(char NumeFisier[8])
{
   char s[500], *p;
   ifstream fIn(NumeFisier);

   while (!(fIn.eof()))
   {
      fIn.getline(s, 500);
//      s.[strlen(s)] = '\0';
      fOut<<s<<"\n";
   }

   return;
}

void main()
{
   int n, i;
   char nume_fis[9] = "vama.in", *aux;

   clrscr();

   cout<<"\n Numarul de puncte de trecere a frontierei : ";
   cin>>n;

   for (i = 1; i <= n; i++)
   {
      nume_fis[7] = '\0';
      aux = itoa(i, aux, 10);
      strcat(nume_fis, aux);
      CitireDinFisier(nume_fis);
   }

   getch();
}