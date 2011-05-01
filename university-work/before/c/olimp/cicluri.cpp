#include <stdio.h>
#include <conio.h>
#include <string.h>
#include <stdlib.h>

int gr[8][8], s[100][100], s1[100];
int m, n, k, u, max= 0;

ok(int pas, int ni)
{
   if (s1[pas] != ni)
      for (int i= 1; i < pas; i++)
	 if (s1[i] == s1[pas])
	    if ((s1[i-1] == s1[pas-1]) || (s1[i+1] == s1[pas-1]))
	       return 0;
   if (s1[pas] == ni)
      if (s1[2] == s1[pas-1])
	 return 0;
   if (gr[s1[pas-1]][s1[pas]] == 0)
      return 0;

   return 1;
}

ok3(int i)
{
      for (int j= 1; s[i][j] != s[i][1]; j++)
	 if (s[i][j] == s1[1])
	    break;
      int v= 1;
      while ((s1[v] != s1[1]) || ((v == 1) && (s1[v] == s1[1])))
      {
	 if (s1[v] != s[i][j])
	    goto etich;
	 v++;
	 j++;
	 if (s[i][j] == s[i][1])
	    j= 1;
      }
      return 1;
      etich:
      v= 1;
      for (j= 1; s[i][j] != s[i][1]; j++)
	 if (s[i][j] == s1[1])
	    break;
      while ((s1[v] != s1[1]) || ((v == 1) && (s1[v] == s1[1])))
      {
	 if (s1[v] != s[i][j])
	    return 0;
	 v++;
	 j--;
	 if (j == 0)
	    for (j= 2; s[i][j+1] != s[i][1]; j++);
      }
      return 1;
   }

ok2()
{
   for (int i= 1; i <= k; i++)
       if (ok3(i))
	  return 0;

   return 1;
}

back(int pas, int ni)
{
   s1[pas]= 0;
   do
   {
      while (s1[pas] < m)
      {
	 s1[pas]++;
	 if (ok(pas, ni))
	    if ((s1[pas] == ni) && (ok2()))
	    {
	       k++;
	       for (u= 1; u <= pas; u++)
	       {
		  s[k][u]= s1[u];
		  printf("%i ", s1[u]);
	       }
	       printf("\n");
	       getch();
	       if (pas > max)
		  max= pas;
	    }
	    else
	    {
	       pas++;
	       s1[pas]= 0;
	    }

      }
      pas--;
   }
   while (pas > 1);

   return 0;
}

main()
{
   int a, b;
   FILE *f;

   clrscr();
   printf("\n");

   for (a= 1; a <= 8; a++)
      for (b= 1; b <= 8; b++)
         gr[a][b]= 0;

   f= fopen("x.in", "r");
   fscanf(f, "%i %i", &m, &n);
   while (!feof(f))
   {
      fscanf(f, "%i %i", &a, &b);
      gr[a][b]= 1;
      gr[b][a]= 1;
   }
   fclose(f);
   k= 0;
   for (int i= 1; i <= m; i++)
   {
      s1[1]= i;
      back(2, i);
   }
/*
   for (i= 1; i <= k; i++)
   {
      for (int j= 1; j <= max; j++)
	 printf("%i ", s[i][j]);
      printf("\n");
      getch();
   }
*/

   return 0;
}