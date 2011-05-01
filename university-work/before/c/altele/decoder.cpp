#include <stdio.h>
#include <stdlib.h>
#include <iostream.h>
#include <conio.h>
#include <string.h>

main()
{
   char s[13];
   char c;

   clrscr();

   cout<<"Numele fisierului de decodat (*.cod) - a nu se introduce extensia : ";
   cin>>s;
   strcat(s, ".cod");

   FILE *f, *g;

   if ((f= fopen(s, "r")) == NULL)
   {
      cout<<"Eroare la deschiderea fisierului de intrare.";
      getch();
      return 0;
   }

   s[strlen(s)-4]= '\0';
   strcat(s, ".dec");
   g= fopen(s, "w");

   int j= 1, i;
   char *x, *a, *p;
   x= new char[200000];

   while (fgets(x, 2000, f))
   {
      x[strlen(x)+1]= '\0';
      a= x;
      while (p= strtok(a, " \n"))
      {
	 a= NULL;
	 i= atoi(p);
	 i= i+j-6;
	 i= i/5;
	 j= i;
	 fprintf(g, "%c", i);
      }
      fprintf(g, "\n");
   }

   fclose(f);
   fclose(g);

   return 0;
}