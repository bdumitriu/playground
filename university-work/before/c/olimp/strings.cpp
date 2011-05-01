#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <conio.h>

max(int value1, int value2)
{
   return ( (value1 > value2) ? value1 : value2);
}

adaugata_sau_stearsa(char *x, char *y)
{
   char aux;

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
	    return 0;
      return 1;
   }
   else
      return 0;

}

modificata(char *x, char *y)
{
   int ct;

   ct= 0;
   if (abs(strlen(x)-strlen(y)) == 0)
   {
      for (int i= 0; i < strlen(y); i++)
	 if (x[i] != y[i])
	    ct++;
   }
   if (ct == 1)
      return 1;
   return 0;
}

exista_legatura(char *x, char *y)
{
   if ((modificata(x, y)) || (adaugata_sau_stearsa(x, y)))
      return 1;
   return 0;
}


main()
{
   char *x, *y;

   clrscr();
   printf("\n");

   x= (char *) malloc(sizeof(char));
   y= (char *) malloc(sizeof(char));
   scanf("%s", x);
   scanf("%s", y);
   if (exista_legatura(x, y))
      printf("DA");
   else
      printf("NU");

   getch();
   return 0;
}