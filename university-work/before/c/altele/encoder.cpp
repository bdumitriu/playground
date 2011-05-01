#include <stdio.h>
#include <iostream.h>
#include <conio.h>
#include <string.h>

main()
{
   char s[13];
   char c;

   clrscr();

   cout<<"Numele fisierului de codat : ";
   cin>>s;

   FILE *f, *g;

   if ((f= fopen(s, "r")) == NULL)
   {
      cout<<"Eroare la deschiderea fisierului de intrare.";
      getch();
      return 0;
   }

   if (s[strlen(s)-4]= '.')
      s[strlen(s)-4]= '\0';
   strcat(s, ".cod");

   g= fopen(s, "w");

   int j= 1;
   while (!feof(f))
   {
      fscanf(f, "%c", &c);
      if (c != '\n')
      {
	 int i= int(c);
	 i= 5*i+6-j;
	 fprintf(g, "%d ", i);
	 j= int(c);
      }
      else
	 fprintf(g, "%c", c);
   }

   fclose(f);
   fclose(g);

   return 0;
}