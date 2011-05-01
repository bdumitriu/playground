/* Program de evaluare a unei expresii */
#include <stdio.h>
#include <ctype.h>
#include <conio.h>
#include <string.h>
#include <alloc.h>
#include <stdlib.h>
float factor();
float termen();
float expr();

int i, poz= 0;
char *sir;
int nr;

float factor()
{
   float e;

   if (isdigit(sir[poz]))
      return sir[poz++] - '0';
   poz++;
   e= expr();
   poz++;
   return e;
}

float termen()
{
   float fs, fd;

   fs= factor();
   switch (sir[poz++])
   {
      case '*':
	 fd= factor();
	 return fs*fd;
      case '/':
	 fd= factor();
	 return fs/fd;
      case '+':
	 fd= factor();
	 return fs+fd;
      case '-':
	 fd= factor();
	 return fs-fd;
      default:
	 poz--;
	 return fs;
   }
}


float expr()
{
   float ts, td;

   ts= termen();
   switch (sir[poz++])
   {
      case '+':
	 td= termen();
	 return ts+td;
      case '-':
	 td= termen();
	 return ts-td;
      default:
	 poz--;
	 return ts;
   }
}

main()
{

   clrscr();
   printf("\n");
   sir= (char *) malloc(50*sizeof(char));
   printf(" EXPRESIA : ");
   gets(sir);
   printf("Expresia are valoarea %f.", expr());
   getch();

   return 0;
}