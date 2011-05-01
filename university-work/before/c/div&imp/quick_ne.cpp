#include <stdio.h>
#include <conio.h>
#include <alloc.h>

int a[100], k;
struct elem {
	  int li, ls;
	  struct elem *l;
       } *varf;

poz(int li, int ls, int *k)
{
   int i1, j1, aux, i, j;

   i1= 0;
   j1= -1;
   i= li;
   j= ls;
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
   }
   *k= i;

   return 0;
}

quick(int li, int ls)
{
   struct elem *p;

   varf->li= li;
   varf->ls= ls;
   while (varf != NULL)
   {
      poz(varf->li, varf->ls, &k);
      p= varf;
      if (li < k-1)
      {
	 p->li= li;
	 p->ls= k-1;
	 if (k+1 < ls)
	 {
	    p= (struct elem*) malloc(sizeof(struct elem));
	    p->li= k+1;
	    p->ls= ls;
	    p->l= NULL;
	    varf= p;
	 }
      }
      else if (k+1 < ls)
      {
	 p->li= k+1;
	 p->ls= ls;
      }
      else
	 varf= varf->l;
   }

   return 0;
}

main()
{
   int i, n;

   clrscr();
   printf("\n");

   printf("Numarul de elemente : ");
   scanf("%i", &n);
   for (i= 1; i <= n; i++)
   {
      printf(" Elementul %i : ", i);
      scanf("%i", &a[i]);
   }
   varf= (struct elem *) malloc(sizeof(struct elem));
   varf->l= NULL;
   quick(1, n);
   printf(" Sirul ordonat : ");
   for (i= 1; i <= n-1; i++)
      printf("%i ", a[i]);
   printf("%i.", a[n]);
   getch();

   return 0;
}