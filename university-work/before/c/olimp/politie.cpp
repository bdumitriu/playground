#include <stdio.h>
#include <conio.h>
#include <stdlib.h>

struct graf
{
   int x, y, v;    /* x, y - cele doua extremitati ale unei muchii */
};
struct lis
{
   int x, a;
   lis *urm;
};
lis *gr[1501], *p, *q;/* in gr vom initializa o matrice de pointeri spre liste */
graf a[5001];   /* in a vom citi initial din fisier extremitatile muchiilor */
int n, m;       /* n - numarul de noduri; m - numarul de muchii */
int s[1501];    /* s il vom folosi pt. solutii */
int nr_pol= 0;     /* nr_pol - numarul de politisi */
FILE *f;

/* procedura creeaza listele de noduri succesive pt. fiecare varf */
liste()
{
    struct pt_b
    {
       int vf, a;
    };
    int i= 0, j, k; /* i il vom folosi ca indice pt. tabloul de pointeri gr */
    pt_b b[1501]; /* om folosi tabloul b pt. a retine in el pe rand nodurile */
		 /* succesive pt. fiecare nod. */

    for (i= 1; i <= n; i++)
    {
       k= 0;
       for (j= 1; j <= m; j++)
       {
	  if (a[j].x == i)
	  {
	     k++;
	     b[k].vf= a[j].y;
	     b[k].a= j;
	  }
	  else
	     if (a[j].y == i)
	     {
		k++;
		b[k].vf= a[j].x;
		b[k].a= j;
	     }
       }
       gr[i]= (lis *) malloc(sizeof(lis));
       gr[i]->urm= NULL;
       p= NULL;
       for (j= 1; j <= k; j++)
       {
	  p= (lis *) malloc(sizeof(lis));
	  if (j == 1)
	  {
	     gr[i]->urm= p;
	     q= p;
	  }
	  else
	  {
	     q->urm= p;
	     q= p;
	  }
	  p->urm= NULL;
	  p->x= b[j].vf;
	  p->a= b[j].a;
       }
    }

    return 0;
}

scrie(int j)
{
   nr_pol++;
   printf(".");
   for (int i= 1; i < j; i++)
   {
      fprintf(f, "%i ", s[i]);
      p= gr[s[i]]->urm;
      while (p->x != s[i+1])
	 p= p->urm;
      a[p->a].v= 1;
   }
   fprintf(f, "%i\n", s[j]);

   return 0;
}

ok(int j, int k)
{
   if ((j == 3) && (s[j] == k))
      return 0;
   for (int i= 2; i < j; i++)
      if (s[i] == s[j])
	 if ((s[i-1] == s[j-1]) || (s[i+1] == s[j-1]))
	    return 0;
   p= gr[s[j-1]]->urm;
   while (p != NULL)
   {
      if (p->x == s[j])
	 return 1;
      p= p->urm;
   }

   return 0;
}

okv(int j)
{
   for (int i= 1; i < j; i++)
   {
      p= gr[s[i]]->urm;
      while (p->x != s[i+1])
	 p= p->urm;
      if (a[p->a].v == 0)
	 return 1;
   }

   return 0;
}

cicluri(int i)
{
   int j= 1;

   s[j]= i;
   j= 2;
   s[j]= i-1;
   while (j > 1)
   {
      while (s[j] < n)
      {
	 s[j]++;
	 if (ok(j, i))
	 {
	    if ((s[j] == i) && (j > 1) && (s[j-1] != s[2]))
	    {
	       if (okv(j))
		  scrie(j);
	    }
	    else
	    {
	       j++;
	       s[j]= i-1;
	    }
	 }
      }
      j--;
   }

   return 0;
}


main()
{
   int i;
   fpos_t *pos;

   /* initializarea ecranului */
   clrscr();
   printf("\n");

   /* deschiderea fisierului */
   if ((f= fopen("input.txt", "r")) == NULL)
   {
      printf(" Eroare la deschiderea fisierului de intrare!");
      getch();
      return 0;
   }

   /* citirea datelor din fisier */
   fscanf(f, "%i %i\n", &n, &m);
   for (i= 1; i <= m; i++)
      fscanf(f, "%i %i\n", &a[i].x, &a[i].y);
   fclose(f);

   liste();
/*
   for (i= 1; i <= n; i++)
   {
      p= gr[i]->urm;
      printf("%i : ", i);
      while (p != NULL)
      {
	 printf("%i,%i ", p->x, p->a);
	 p= p->urm;
      }
      printf("\n");
   }
   getch();
   return 0;
*/

   /* crearea fisierului de iesire */
   if ((f= fopen("output.txt", "w")) == NULL)
   {
      printf(" Eroare la deschiderea fisierului de iesire!");
      getch();
      return 0;
   }
   fprintf(f, "\n");
/*
   fprintf(f, "1 2 3");
   fclose(f);
   return 0;
*/

   /* rezolvarea propriu-zisa a problemei */
   for (i= 1; i <= n; i++)
      cicluri(i);
/*
   fclose(f);
   if ((f= fopen("output.txt", "w")) == NULL)
   {
      printf(" Eroare la deschiderea fisierului de iesire!");
      getch();
      return 0;
   }
*/
   fseek(f, 0, 0);
   fprintf(f, "%i", nr_pol);
   fclose(f);

   /* incheierea programului */
//   getch();
   return 0;
}