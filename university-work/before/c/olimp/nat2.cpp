#include <stdio.h>
#include <conio.h>

FILE *f;
int n[61], l, x, tst, aux, m;
int s[61];

scrie(int i)
{
   fprintf(f, "%i\n", i);
   for (int j= 1; j <= i; j++)
      fprintf(f, "%i ", n[s[j]]);

   return 0; 
}

suma(int i)
{
   int ss;

   ss= 0;
   for (int j= 1; j <= i; j++)
      ss+= n[s[j]];

   return ss;
}

ok (int i)
{
   if ((i > 1) && (s[i] <= s[i-1]))
      return 0;

   return 1;
}

back(int *test)
{
   int i, tt;

   *test= 0;
   i= 1;
   s[i]= 0;
   tt= 1;
   do
   {
      while (s[i] < m)
      {
	 s[i]++;
	 if (ok(i))
	    if (suma(i) == l)
	    {
	       scrie(i);
	       *test= 1;
	       s[i]= m;
	       tt= 0;
	    }
	    else
	    {
	       i++;
	       s[i]= 0;
	    }
      }
      i--;
   }
   while ((i > 0) && (tt == 1));

   return 0;
}

main()
{
   int i, t;

   clrscr();
   printf("\n");

   if ((f= fopen("nat2.in", "r")) == NULL)
   {
      printf(" Eroare la deschiderea fisierului de intrare.");
      getch();
      return 0;
   }
   fscanf(f, "%i\n", &l);
   i= 0;
   do
   {
      i++;
      x= fscanf(f, "%i", &n[i]);
   }
   while (x > 0);

   i--;
   m= i;
   while (x > 0);
   do
   {
      tst= 1;
      for (i= 1; i< m; i++)
	 if (n[i] > n[i+1])
	 {
	    tst= 0;
	    aux= n[i];
	    n[i]= n[i+1];
	    n[i+1]= aux;
	 }
   }
   while (tst == 0);
   fclose(f);
   if ((f= fopen("nat2.out", "w")) == NULL)
   {
      printf(" Eroare la crearea fisierului de iesire.");
      getch();
      return 0;
   }
   back(&t);
   if (t == 0)
      fprintf(f, "NU");
   fclose(f);

   getch();
   return 0;
}