#include <stdio.h>
#include <conio.h>
#define max 100
int a[max], i, n;

comb(int inf, int med, int sup)
{
   int i, j, k, l;
   int b[max];

   i= inf;
   j= med+1;
   k= inf;
   while ((i <= med) && (j <= sup))
   {
      if (a[i] <= a[j])
      {
	 b[k]= a[i];
	 i= i+1;
      }
      else
      {
	 b[k]= a[j];
	 j= j+1;
      }
      k= k+1;
   }
   for (l= i; l <= med; l=l+1)
   {
      b[k]= a[l];
      k= k+1;
   }
   for (l= j; l <= sup; l=l+1)
   {
      b[k]= a[l];
      k= k+1;
   }
   for (l= inf; l <= sup; l=l+1)
      a[l]= b[l];

   return 0;
}

sortare(int inf, int sup)
{
   int med;

   if (inf < sup)
   {
      med= int((inf+sup)/2);
      sortare(inf, med);
      sortare(med+1, sup);
      comb(inf, med, sup);
   }

   return 0;
}

main()
{
   clrscr();
   printf("\n");

   printf(" Numarul de elemente : ");
   scanf("%i", &n);
   for (i= 1; i <= n; i= i+1)
   {
      printf("  Elementul %i : ", i);
      scanf("%i", &a[i]);
   }
   printf(" Sirul dat initial : ");
   for (i= 1; i <= n; i= i+1)
      printf("%i ", a[i]);
   printf(".\n");
   printf(" Sirul sortat : ");
   sortare(1, n);
   for (i= 1; i <= n; i= i+1)
   printf("%i ", a[i]);
   printf(".\n");

   getch();

   return 0;
}