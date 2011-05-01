#include <stdio.h>
#include <stdlib.h>
#include <conio.h>

main()
{
   int i, n, s[1001], nx, ny, sum;
   static int x[1001], y[1001], b[1001][4];
   static int sol[1001];
   FILE *f;

   clrscr();
   printf("\n");

   if ((f= fopen("input.txt", "r")) == NULL);
   {
      printf("Eroare la deschiderea fisierului de intrare!");
      getch();
      exit;
   }
   printf(" Citesc datele din fisierul de intrare...");
   fscanf(f, "%i\n", &n);
   for (i= 1; i <= n; i++)
   {
      fscanf(f, "%i", &x[i]);
      if (x[i] == 0)
      {
	 nx= i-1;
	 i= n+1;
	 fscanf(f, "\n");
      }
   }
   for (i= 1; i <= n; i++)
   {
      fscanf(f, "%i", &y[i]);
      if (y[i] == 0)
      {
	 ny= i-1;
	 i= n+1;
	 fscanf(f, "\n");
      }
   }
   for (i= 1; i <= n; i++)
      fscanf(f, "%i", &s[i]);
   fclose(f);
   printf(" Am terminat de citit datele din fisier.");
   printf(" Rezolv problema...");
   if ((f= fopen("output.txt", "w")) == NULL);
   {
      printf("Eroare la deschiderea fisierului de iesire!");
      getch();
      exit;
   }
   for (i= 1; i <= nx; i++)
      b[x[i]][1]= 1;
   for (i= 1; i <= ny; i++)
      b[y[i]][2]= 1;
   for (i= 1; i <= n; i++)
      b[i][3]= s[i];
   for (i= 1; i <= n; i++)
      if ((b[i][1] == 1) && (b[i][2] == 1))
      {
	 fprintf(f, "0\n");
	 fprintf(f, "#");
	 fclose(f);
	 exit;
      }
   for (i= 1; i < n; i++)
      if (((b[i][1] == 1) && (b[i+1][2] == 1)) || ((b[i][2] == 1) && (b[i+1][1] == 1)))
      {
	 fprintf(f, "0\n");
	 fprintf(f, "#");
	 fclose(f);
	 exit;
      }
   sum= 0;
   int k= 0;
   for (i= 1; i <= n; i++)
   {
      int aux= 0;
      for (int j= i; j <= n; j++)
	 if ((b[j][1] == 0) && (b[j][2] == 0))
	    aux++;
	 else
	    if (aux == 0)
	    {
	       if (b[j][1] == 1)
	       {
		  k++
		  s[k]= 1;
	       }
	       else
	       {
		  k++;
		  s[k]= -1;
	       }
	       j= n+1;
	    }
      if (aux == 1)
	 if (b[i+1][1] == 1)
	 {
	    k++;
	    sol[k]= -1;
	 }
	 else
	 {
	    k++;
	    sol[k]= 1;
	 }
      if (aux > 1)
      {

      }
   }

   return 0;
}