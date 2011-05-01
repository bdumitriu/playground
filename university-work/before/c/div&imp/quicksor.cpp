/* sortarea rapida */
#include <stdio.h>
#include <conio.h>
int k;

poz(int li, int ls, int *k, int a[100])
{
   int i, j, i1, j1, aux;

   i= li;
   j= ls;
   i1= 0;
   j1= -1;
   while (i < j)
   {
      if (a[i] > a[j])
      {
	 aux= a[i];
	 a[i]= a[j];
	 a[j]= aux;
	 aux= i1;
	 i1= -j1;
	 j1= -aux;
      }
      i= i+i1;
      j= j+j1;
      if (i == j)
	 *k= i;
   }

   return 0;
}

quick(int li, int ls, int a[100])
{
   if (li < ls)
   {
      poz(li, ls, &k, a);
      quick(li, k-1, a);
      quick(k+1, ls, a);
   }

   return 0;
}

main()
{
   int n, i, a[100];

   clrscr();
   printf("\n");

   printf(" Numarul de elemente din vector : ");
   scanf("%i", &n);
   for (i= 1; i <= n; i= i+1)
   {
      printf("  Elementul %i : ", i);
      scanf("%i", &a[i]);
   }
   quick(1, n, a);
   printf(" Sirul ordonat este : ");
   for (i= 1; i <= n; i= i+1)
      printf("%i ", a[i]);

   getch();
   return 0;
}