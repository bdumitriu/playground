#include <stdio.h>
#include <conio.h>
#include <string.h>

FILE *f;
char c[1000];
int i, n, m, tst;

main()
{
   clrscr();
   printf("\n");

   if ((f= fopen("nat5.in", "r")) == NULL)
   {
      printf(" Eroare la deschiderea fisierului de intrare.");
      getch();
      return 0;
   }
   c[0]='\0';
   fscanf(f, "%s", c);
   fclose(f);
   if ((f= fopen("nat5.out", "w")) == NULL)
   {
      printf(" Eroare la crearea fisierului de iesire.");
      getch();
      return 0;
   }
   i= 0;
   n= strlen(c);
   while (i < n)
   {
      tst= 0;
      m= (c[i+1]-'0')*10;
      m+= c[i]-'0';
      if (m >= 32)
	 tst= 2;
      else
	 tst= 1;
      if (tst == 1)
      {
	 if (i+2 < n)
	 {
	    m+= (c[i+2]-'0')*100;
	    if (m <= 255)
	       tst= 2;
	    else
	       tst= 0;
	 }
	 else
	    tst= 0;
      }
      else
	 if (tst == 2)
	 {
	    if (i+2 < n)
	    {
	       m+= (c[i+2]-'0')*100;
	       if (m > 255)
		  tst= 1;
	       else
		  tst= 0;
	    }
	    else
	       tst= 1;
	 }
     if (tst == 0)
     {
	fclose(f);
	f= fopen("nat5.out", "w");
	fprintf(f, " Nu se poate reface textul initial.");
	i= n;
     }
     if (tst == 1)
     {
	m= m%100;
	fprintf(f, "%c", m);
	i= i+2;
     }
     if (tst == 2)
     {
       fprintf(f, "%c", m);
       i= i+3;
     }
   }
   fclose(f);

   getch();
   return 0;
}