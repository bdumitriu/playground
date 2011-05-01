#include <stdio.h>
#include <conio.h>

FILE *f;
int x1[101], y1[101], x2[101], y2[101], n;

aflarea_ariei()
{
   int i, j, s, aux, t;

   s= 0;
   for (i= 1; i <= n; i++)
   {
      for (j= i+1; j <= n; j++)
      {
	 if (x1[i] > x1[j])
	 {
	    if ()
	 }
      }
   }

   return 0;
}

void main()
{
   int i;

   clrscr();
   printf("\n");

   if ((f= fopen("dr.in", "r")) == NULL)
   {
      printf(" Eroare la deschiderea fisierului de intrare.");
      getch();
      return;
   }
   i= 0;
   while (!feof(f))
   {
      i++;
      fscanf(f, "%i %i %i %i\n", &x1[i], &y1[i], &x2[i], &y2[i]);
   }
   n= i;
   aflarea_ariei();

   return;
}