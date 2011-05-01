#include <stdio.h>
#include <conio.h>
#include <string.h>
static int shift[256];

bm_init(char *p, int m)
{
   int i;

   for (i= 0; i < 256; i++)
      shift[i]= m;
   for (i= 1; i < m; i++)
      shift[(unsigned char) p[i-1]]= m-i;
   return 0;
}

bm_search(char *t, char *p, int m)
{
   register int i, j, k;

   for (i= j= m-1; j >= 0; i--, j--)
      while (t[i] != p[j])
      {
	 k= shift[(unsigned char) t[i]];
	 i+= (m-j > k)?m-j:k;
	 j= m-1;
      }

   return i+1;
}

main()
{
   char *p, *t;
   int m, poz;

   clrscr();
   printf("\n");
   printf("Textul in care se face cautarea : ");
   gets(p);
   printf("Textul de cautat : ");
   gets(t);
   m= strlen(t);
   bm_init(t, m);
   poz= bm_search(p, t, m);
   printf("Textul %s se afla la pozitia %i in textul initial.", t, poz);
   getch();

   return 0;
}