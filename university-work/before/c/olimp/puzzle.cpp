#include <stdio.h>
#include <conio.h>
#include <process.h>
int a[5][5] = {0, 0, 0, 0, 0,
	       0, 7 ,16, 9, 1,
	       0, 2, 11, 14, 5,
	       0, 10, 8, 6, 12,
	       0, 4, 3, 13, 15};
int a1[5][5];
int s[100];

aranjate()
{
   int i, j;

   for (i= 1; i <= 4; i++)
      for (j= 1; j <= 4; j++)
      {
	 if ((i-1)*4+j != a1[i][j]) return 0;
      }

   return 1;
}

roteste(int i)
{
   int aux;

   switch (i)
   {
    case 1:
    {
       aux= a1[1][1]; a1[1][1]= a1[2][1]; a1[2][1]= a1[3][1];
       a1[3][1]= a1[3][2]; a1[3][2]= a1[3][3]; a1[3][3]= a1[2][3];
       a1[2][3]= a1[1][3]; a1[1][3]= a1[1][2]; a1[1][2]= aux;
    } break;
    case 2:
    {
       aux= a1[1][2]; a1[1][2]= a1[2][2]; a1[2][2]= a1[3][2];
       a1[3][2]= a1[3][3]; a1[3][3]= a1[3][4]; a1[3][4]= a1[2][4];
       a1[2][4]= a1[1][4]; a1[1][4]= a1[1][3]; a1[1][3]= aux;
    } break;
    case 3:
    {
       aux= a1[2][1]; a1[2][1]= a1[3][1]; a1[3][1]= a1[4][1];
       a1[4][1]= a1[4][2]; a1[4][2]= a1[4][3]; a1[4][3]= a1[3][3];
       a1[3][3]= a1[2][3]; a1[2][3]= a1[2][2]; a1[2][2]= aux;
    } break;
    case 4:
    {
       aux= a1[2][2]; a1[2][2]= a1[3][2]; a1[3][2]= a1[4][2];
       a1[4][2]= a1[4][3]; a1[4][3]= a1[4][4]; a1[4][4]= a1[3][4];
       a1[3][4]= a1[2][4]; a1[2][4]= a1[2][3]; a1[2][3]= aux;
    } break;
   }

   return 0;
}

verifica(int i)
{
   int j;

   for (j= 1; j <= i; j++)
      roteste(s[j]);

   return 0;
}

scrie(int i)
{
   int j;

   printf(" Succesiunea de apasari este : ");
   for (j= 1; j <= i; j++)
   {
      switch (s[j])
      {
       case 1: printf("A "); break;
       case 2: printf("B "); break;
       case 3: printf("C "); break;
       case 4: printf("D "); break;
      }
   }

   return 0;
}

back(int n)
{
   int i, j, k;
   int test, status;
//   int a2[5][5];

   i= 1;
   s[1]= 0;
   do
   {
      while (s[i] < 4)
      {
	 s[i]+= 1;
	 if (i == n)
	 {
	   // for (j= 1; j <= 4; j++)
	     //  for (k= 1; k <= 4; k++)
	       //	  a2[j][k]= a1[j][k];
	    verifica(i);
	    if (aranjate())
	    {
	       scrie(i);
	       getch();
	       exit(status);
	    }
	    else
	       for (j= 1; j <= 4; j++)
		  for (k= 1; k <= 4; k++)
		     a1[j][k]= a[j][k];
	 }
	 else
	 {
	    i= i+1;
	    s[i]= 0;
	 }

      }
      i--;
   }
   while (i > 0);

   return 0;
}

citirea_datelor()
{
   int i, j;

   printf(" Introduceti configuratia initiala a puzzle-ului :\n");
   for (i= 1; i <= 4; i++)
      for (j= 1; j <= 4; j++)
      {
	 printf("Elementul %i,%i : ", i, j);
	 scanf("%i", &a[i][j]);
      }

   return 0;
}

main()
{
   int i, j, k;

   clrscr();
   printf("\n");

//   citirea_datelor();
   k= 1;
   if (aranjate())
      printf("Puzzle-ul este deja aranjat.");
   else
      do
      {
	 for (i= 1; i <= 4; i++)
	    for (j= 1; j <= 4; j++)
	       a1[i][j]= a[i][j];
	 back(k);
	 k= k+1;
      }
      while (!aranjate());

   return 0;
}