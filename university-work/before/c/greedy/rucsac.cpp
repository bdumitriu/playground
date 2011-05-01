/* problema rucsacului */
#include <stdio.h>
#include <conio.h>
struct obiect
{
   int u;
   float g;
} ob[100];

citirea_datelor(int *n, float *g)
{
   int i;
   float c;

   printf(" Capacitatea rucsacului (cantitatea totala ce o poate duce) : ");
   scanf("%i", g);
   printf(" Numarul de obiecte : ");
   scanf("%i", n);
   for (i= 1; i <= *n; i= i+1)
   {
      printf("  greutatea obiectului %i : ", i);
      scanf("%f", &c);
      ob[i].g= c;
      printf("  utilitatea obiectului %i : ", i);
      scanf("%i", &ob[i].u);
   }

   return 0;
}

inord(int n)
{
   int i;

   for (i= 1; i <= n-1; i= i+1)
      if (ob[i].u/ob[i].g < ob[i+1].u/ob[i+1].g)
	 return 1;
   return 0;
}

ordonarea_obiectelor(int n)
{
   int i;
   obiect aux;

   while (inord(n))
   {
   for (i= 1; i <= n-1; i= i+1)
      if (ob[i].u/ob[i].g < ob[i+1].u/ob[i+1].g)
      {
	 aux= ob[i];
	 ob[i]= ob[i+1];
	 ob[i+1]= aux;
      }
   }

   return 0;
}

float greutatea_totala(int n)
{
   int i;
   float x;

   x= 0;
   for (i= 1; i <= n; i= i+1)
      x= x+ob[i].g;

   return x;
}

crearea_solutiei(int n, float g)
{
   int i;

   printf("Se pun : \n");
   for (i= 1; i <= n; i= i+1)
   if (greutatea_totala(i-1) < g)
      printf(" - obiectul cu greutatea %.2f si cu utilitatea %i", ob[i].g, ob[i].u);

   return 0;
}

main()
{
   int n;
   float g;

   clrscr();
   printf("\n");

   citirea_datelor(&n, &g);
   ordonarea_obiectelor(n);
   crearea_solutiei(n, g);

   getch();
   return 0;
}
