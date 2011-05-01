#include <stdio.h>
#include <conio.h>
#include <stdlib.h>

main()
{
   struct xx
   {
      int c, g, n;
   } s[1000];
   FILE *f;
   int n, c[1000], c1[1000], p, g[1000], g1[1000], i, v[1000];

   clrscr();
   printf("\n");

   c[0]= c1[0]= g[0]= g1[0]= 0;
   if ((f= fopen("input.txt", "r")) == NULL)
   {
      printf("Eroare la deschiderea fisierului de intrare.");
      getch();
      exit;
   }
   printf(" Citesc datele de intrare din fisier...\n");
   fscanf(f, "%i\n", &n);
   for (i= 1; i <= n; i++)
      fscanf(f, "%i", &c[i]);
   fscanf(f, "%i\n", &p);
   for (i= 1; i <= p; i++)
      fscanf(f, "%i", &g[i]);
   fclose(f);
   printf(" Am terminat de citit datele din fisier.\n");
   printf(" Rezolv problema...\n");
   for (i= 1; i <= n; i++)
      c1[i]= c[i];
   int aux= 1;
   while (aux == 1)
   {
      aux= 0;
      for (i= 1; i < n; i++)
	 if (c[i] > c[i+1])
	 {
	    aux= c[i];
	    c[i]= c[i+1];
	    c[i+1]= aux;
	    aux= 1;
	 }
   }
   for (i= 1; i <= p; i++)
      g1[i]= g[i];
   aux= 1;
   while (aux == 1)
   {
      aux= 0;
      for (i= 1; i < p; i++)
	 if (g[i] < g[i+1])
	 {
	    aux= g[i];
	    g[i]= g[i+1];
	    g[i+1]= aux;
	    aux= 1;
	 }
   }
   if ((f= fopen("output.txt", "w")) == NULL)
   {
      printf("Eroare la deschiderea fisierului de iesire.");
      getch();
      exit;
   }
   int k= 1;
   int cap= 0;
   for (i= 1; i <= p; i++)
      v[i]= 0;
   for (i= 1; i <= n; i++)
   {
      for (int j= 1; j <= p; j++)
	 if ((c[i]-g[j] >= 0) && (v[j] == 0))
	 {
	    s[k].c= c[i];
	    s[k].g= g[j];
	    v[j]= 1;
	    k++;
	    cap+= c[i]-g[j];
	    j= p+1;
	 }
   }
   for (i= 1; i <= n; i++)
      c[i]= c1[i];
   for (i= 1; i <= p; i++)
      g[i]= g1[i];
   for (i= 1; i <= n; i++)
   {
      aux= 0;
      for (int j= 1; j <= k-1; j++)
	 if (s[j].c == c[i])
	    aux= 1;
      if (!aux)
	 cap+= c[i];
   }
   for (i= 1; i <= p; i++)
      for (int j= 1; j <= k; j++)
	 if (s[j].g == g[i])
	    s[j].n= i;
   fprintf(f, "%i\n", cap);
   for (i= 1; i <= n; i++)
   {
      aux= 0;
      for (int j= 1; j <= k-1; j++)
	 if (s[j].c == c[i])
	 {
	    fprintf(f, "%i %i %i\n", s[j].c, s[j].n, s[j].g);
	    aux= 1;
	    j= k;
	 }
      if (!aux)
	 fprintf(f, "%i 0 0\n", c[i]);
   }
   fclose(f);

   getch();

   return 0;
}