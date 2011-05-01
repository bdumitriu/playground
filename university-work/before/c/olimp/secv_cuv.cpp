/* Problema cu cele -> 5010 de cuvinte */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <alloc.h>
#include <conio.h>
#include <io.h>

struct nod
       {
	  char leg;     // leg=1 daca exista legatura intre cele doua altfel 0
	  char nr;      // nr. de cuvinte prin care am trecut
       };

char cuv[501][20];

struct nod huge t[500][500];

int s[501];
int n;

max(int value1, int value2)
{
   return ( (value1 > value2) ? value1 : value2);
}

adaugata_sau_stearsa(int i, int j)
{
   char *x, *y, aux;
   char g, *h;

   x= (char *) malloc(20*sizeof(char));
   y= (char *) malloc(20*sizeof(char));
   for (int k= 0; k < strlen(cuv[i]); k++)
      x[k]= cuv[i][k];
   x[k]= '\0';
   for (k= 0; k < strlen(cuv[j]); k++)
      y[k]= cuv[j][k];
   y[k]= '\0';
   if (abs(strlen(x)-strlen(y)) == 1)
   {
      int test= (strlen(x) > strlen(y))?1:2;
      int m= max(strlen(x), strlen(y));
      for (int i= 0; i < m; i++)
	 if (x[i] != y[i])
	 {
	    if (test == 1)
	    {
	       for (int j= i; j < strlen(x)-1; j++)
	       {
		  aux= x[j+1];
		  x[j]= aux;
	       }
	    }
	    if (test == 2)
	    {
	       for (int j= i; j < strlen(y)-1; j++)
	       {
		  aux= y[j+1];
		  y[j]= aux;
	       }
	    }
	    i= 100;
	 }
      for (i= 0; i < m-1; i++)
	 if (x[i] != y[i])
	 {
	    free(x);
	    free(y);
	    return 0;
	 }
      free(x);
      free(y);
      return 1;
   }
   else
   {
      free(x);
      free(y);
      return 0;
   }
}

modificata(int i, int j)
{
   char *x, *y, g, *h;
   int ct;

   x= (char *) malloc(20*sizeof(char));
   y= (char *) malloc(20*sizeof(char));
   for (int k= 0; k < strlen(cuv[i]); k++)
      x[k]= cuv[i][k];
   x[k]= '\0';
   for (k= 0; k < strlen(cuv[j]); k++)
      y[k]= cuv[j][k];
   y[k]= '\0';
   ct= 0;
   if (abs(strlen(x)-strlen(y)) == 0)
   {
      for (int i= 0; i < strlen(y); i++)
	 if (x[i] != y[i])
	    ct++;
   }
   free(x);
   free(y);
   if (ct == 1)
      return 1;
   return 0;
}

exista_legatura(int i, int j)
{
   if ((modificata(i, j)) || (adaugata_sau_stearsa(i, j)))
      return 1;
   return 0;
}

alocare()
{
   int i;
/*
//   t = (nod *) malloc(sizeof(nod)*501);
   for (i= 1; i <= 501; i++)
      t[i]= (nod *) malloc(sizeof(struct nod)*501);
*/
   for (i= 1; i <= n; i++)
      for (int j= 1; j <=n; j++)
      {
	 t[i][j].leg= 0;
	 t[i][j].nr= 0;
      }

   return 0;
}

afl_secv()
{
   int m, k;

   for (int i= 2; i <= n; i++)
      for (int j= 1; j < i; j++)
      {
	 if (t[i][j].leg == 1)
	 {
	    m= 0;
	    for (k= 1; k < j; k++)
	       if (t[j][k].nr > m)
		  m= t[j][k].nr;
	    m++;
	    t[i][j].nr= m;
	 }
      }
   return 0;
}

afl_drumului()
{
   int m= 0;
   int ii= 0;
   int jj= 0;
   for (int i= 1; i <= n; i++)
      for (int j= 1; j < i; j++)
      {
	 if (t[i][j].nr > m)
	 {
	    m= t[i][j].nr;
	    ii= i;
	    jj= j;
	 }
      }
   i= ii;
   int j= jj;
   s[1]= i;
   int st= t[i][j].nr;
   for (int k= 1; k <= st; k++)
   {
      s[k+1]= j;
      for (int l= 1; l < j; l++)
	 if (t[j][l].nr == t[i][j].nr-1)
	 {
	    i= j;
	    j= l;
	    l= 501;
	 }
   }
   return st+1;
}

main()
{
   int i;
   char x[12], y[12];
   FILE *f;

   clrscr();
   printf("\n");

   printf(" Numele fisierului de intrare : ");
   scanf("%s", x);
   printf(" Numele fisierului de iesire : ");
   scanf("%s", y);
   if ((f= fopen(x, "r")) == NULL)
   {
      printf("Eroare la deschiderea fisierului de intrare %s.", x);
      getch();
      return 0;
   }
   i= 0;
   while (!feof(f))
   {
      i++;
      fscanf(f, "%s\n", cuv[i]);
   }
   n= i;
   alocare();
   getch();

   for (i= 1; i <= n; i++)
      for (int j= 1; j < i; j++)
	 if (exista_legatura(i, j))
	 {
	    t[i][j].leg= 1;
	 }
   afl_secv();
   getch();
   int k= afl_drumului();
   fclose(f);
   f= fopen(y, "w");
   fprintf(f, "%i\n", k);
   for (i= k; i > 0; i--)
      fprintf(f, "%s\n", cuv[s[i]]);
   fclose(f);

   getch();
   return 0;
}