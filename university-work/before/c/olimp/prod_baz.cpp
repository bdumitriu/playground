#include <stdio.h>
#include <string.h>
#include <conio.h>
#include <values.h>
#include <stdlib.h>

char x[100], y[100], *r;
int p;

char* inmultire(char *n1, char *n2)
{
   char sume[100][200], rez[200];
   int nr_sume= strlen(n1);
   int transport= 0, i, j, k;
   int produs, suma;

   for (i= nr_sume-1, k= 0; i >= 0; i--, k++)
   {
      for (j= strlen(n2)-1; j >= 0; j--)
      {
	 produs= (n1[i]-'0')*(n2[j]-'0');
	 produs+= transport;
	 sume[k][strlen(n2)-j+k+1]= produs%p;
	 transport= (int)produs/p;
      }
      sume[k][strlen(n2)+k]= '\0';
   }

   transport= 0;
   for (i= 0; i < strlen(sume[nr_sume]); i++)
   {
      suma= 0;
      for (j= 0; j < nr_sume; j++)
	 suma+= sume[j][i]-'0';
      suma+= transport;
      rez[i]= suma%p;
      transport= suma/p;
   }

   return rez;
}

main()
{
   clrscr();
   printf("\n");

   printf(" Baza : ");
   scanf("%i", &p);
   printf(" Primul numar : ");
   gets(x);
   gets(x);
   printf(" Al doilea numar : ");
   gets(y);
   r= (char *) malloc(200*sizeof(char));
   r= inmultire(x, y);
   r[strlen(r)]= '\0';
   printf("  Produsul celor doua numere este %s.", r);

   getch();
   return 0;
}