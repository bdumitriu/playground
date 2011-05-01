#include <stdio.h>
#include <conio.h>

static int t[100];
static int ind= 0;
       
void adaug(int x)
{
   t[ind++]= x;
}

void scot(int x)
{
   for (int i= 0; i < ind; i++)
      if (t[i] == x)
      {
	 for(int j=i; j<ind-1; j++ )
	    t[j] = t[j+1];
	 ind=ind-1;
	 break;
      }
}

void list()
{
   printf("Tabloul : ");
   for (int i= 0; i < ind; i++)
      printf("%i ", t[i]);
   getch();
}