#include <stdio.h>
#include <conio.h>

cautare_binara(int n, int a[100], float x)
{
   int p, u, i;

   p= 1;
   u= n;
   while (p <= u)
   {
      i= int((p+u)/2);
      if (x == a[i])
	 return i;
      else
	 if (x < a[i])
	    u= i-1;
	 else
	    p= i+1;
   }

  return 0;
}

main()
{
   int s, i, n;
   int a[100];
   float x;

   clrscr();
   printf("\n");

   printf(" Numarul de elemente : ");
   scanf("%i", &s);
   for (i= 1; i <= s; i= i+1)
   {
      printf("  Elementul %i : ", i);
      scanf("%i", &a[i]);
   }
   printf(" Elementul ce trebuie cautat : ");
   scanf("%f", &x);

   n= cautare_binara(s, a, x);
   if (n != 0)
      printf(" Elementul exista in sir pe pozitia %i.", n);
   else
      printf(" Elementul nu exista in sir.");

   getch();

   return 0;
}