#include <stdio.h>
#include <conio.h>
#include <string.h>

#include <bogdan.h>

void meniu(int x, int y, char a[100][50], int n, int cul_chenar, int cul_cursor, int cul_text, int cul_back)
{
   int i, max;
		     //cul_chenar, cursor, text, back
   textbackground(cul_back);
   textcolor(cul_chenar);
   gotoxy(x, y);
   printf("�");
   max= 0;
   for (i= 1; i <= n; i++)
      if (strlen(a[i]) > max)
	 max= strlen(a[i]);
   for (i= 1; i <= max; i++)
      printf("�");
   printf("�");
   for (i= y+1; i <= y+n; i++)
   {
      gotoxy(x, i);
      printf("�");
      gotoxy(x+1+max, i);
      printf("�");
   }
   gotoxy(x, y+n+1);
   printf("�");
   for (i= 1; i <= max; i++)
      printf("�");
   printf("�");

   return;
}