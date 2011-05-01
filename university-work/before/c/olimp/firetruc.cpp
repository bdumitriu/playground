#include <stdio.h>
#include <conio.h>

int gr[22][22];
int vf_fin, n, nr_sol;
int s[22];
FILE *f, *g;

scrie(int i)
{
   int j;

   nr_sol++;
   for (j= 1; j <= i; j++)
      fprintf(g, "%i ", s[j]);
   fprintf(g, "\n");

   return 0;
}

ok(int i)
{
   for (int j= 1; j < i; j++)
      if (s[j] == s[i])
	 return 0;
   if ((i > 1) && (gr[s[i]][s[i-1]] == 0))
      return 0;

   return 1;
}

sol()
{
   int i;

   s[1]= 1;
   i= 2;
   s[i]= 0;
   while (i > 1)
   {
      while (s[i] < n)
      {
	 s[i]++;
	 if (ok(i))
	 {
	    if (s[i] == vf_fin)
	       scrie(i);
	    else
	    {
	       i++;
	       s[i]= 0;
	    }
	 }
      }
      i--;
   }

   return 0;
}

init_gr()
{
   for (int i= 1; i <= 21; i++)
      for (int j= 1; j <= 21; j++)
	 gr[i][j]= 0;

   return 0;
}

main()
{
   char *x, *y;
   int a, b;

   clrscr();
   printf(" Name of the input file : ");
   gets(x);
   printf(" Name of the output file : ");
   gets(y);
   if ((f= fopen(x, "r")) == NULL)
   {
      printf("Error opening the input file !");
      getch();
      return 0;
   }
   if ((g= fopen(y, "w")) == NULL)
   {
      printf("Error opening the output file !");
      getch();
      return 0;
   }
   int k= 0;
   do
   {
      init_gr();
      fscanf(f, "%i\n", &vf_fin);
      n= 1;
      do
      {
	 fscanf(f, "%i %i\n", &a, &b);
	 if (a > n)
	    n= a;
	 if (b > n)
	    n= b;
	 if ((a > 0) && (b > 0))
	 {
	    gr[a][b]= 1;
	    gr[b][a]= 1;
	 }
      }
      while ((a != 0) || (b != 0));
      k++;
      fprintf(g, "CASE %i:\n", k);
      nr_sol= 0;
      sol();
      fprintf(g, "There are %i routes from the firestation to streetcorner %i.\n", nr_sol, vf_fin);
   }
   while (!feof(f));
   fclose(f);
   fclose(g);

   return 0;
}