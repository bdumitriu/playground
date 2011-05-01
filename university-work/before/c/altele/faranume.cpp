#include <stdio.h>
#include <conio.h>
#include <string.h>
#include <graphics.h>
#include <dos.h>
//#include <c:\bc31\work\surse\olimp\bogdan.h>

int meniu(int x, int y, char a[100][50], int n, int cul_chenar, int cul_cursor, int cul_text, int cul_back, int cul_high, int y_init)
{
   int i, max, m;
   char c;
		     //cul_chenar, cursor, text, back
   textbackground(cul_back);
   textcolor(cul_chenar);
   gotoxy(x, y);
   cprintf("Ú");
   max= 0;
   for (i= 1; i <= n; i++)
      if (strlen(a[i]) > max)
	 max= strlen(a[i]);
   for (i= 1; i <= max+2; i++)
      cprintf("Ä");
   cprintf("¿");
   for (i= y+1; i <= y+n; i++)
   {
      gotoxy(x, i);
      cprintf("³");
      gotoxy(x+3+max, i);
      cprintf("³");
   }
   gotoxy(x, y+n+1);
   cprintf("À");
   for (i= 1; i <= max+2; i++)
      cprintf("Ä");
   cprintf("Ù");
   textcolor(cul_text);
   for (i= 1; i <= n; i++)
   {
      gotoxy(x+2, y+i);
      cprintf("%s", a[i]);
   }
   gotoxy(x+1, y+y_init);
   textcolor(cul_cursor);
   for (i= 1; i <= max+2; i++)
      cprintf("Û");
   textbackground(cul_cursor);
   textcolor(cul_high);
   gotoxy(x+2, y+y_init);
   cprintf("%s", a[y_init]);
   m= y_init;
   do
   {
      c= getch();
      if (c == 0)
      {
	 c= getch();
	 gotoxy(x+1, y+m);
	 textcolor(cul_back);
	 for (i= 1; i <= max+2; i++)
	    cprintf("Û");
	 gotoxy(x+2, y+m);
	 textbackground(cul_back);
	 textcolor(cul_text);
	 cprintf("%s", a[m]);
	 if (c == 72)
	 {
	    m--;
	    if (m == 0)
	       m= n;
	 }
	 else
	 {
	    m++;
	    if (m == n+1)
	       m= 1;
	 }
	 textcolor(cul_cursor);
	 gotoxy(x+1, y+m);
	 for (i= 1; i <= max+2; i++)
	    cprintf("Û");
	 gotoxy(x+2, y+m);
	 textbackground(cul_cursor);
	 textcolor(cul_high);
	 cprintf("%s", a[m]);
      }
   }
   while (c != 13);

   return m;
}

main()
{
   int gdriver= DETECT, gmode, i, j, ii, jj, iii[500], b[500], s[500];
   int n= 3, x= 25, y= 10, m;
   int cul_text= 2, cul_cursor= 5, cul_chenar= 4, cul_back= 0, cul_high= 11;
   char a[100][50];

   textbackground(BLACK);
   textcolor(LIGHTGRAY);
   clrscr();

   gotoxy(20, 8);
   printf("Selectati ce doriti sa vedeti : ");
   a[1][0]= '\0';
   strcat(a[1], "Punct miscandu-se pe ecran");
   a[2][0]= '\0';
   strcat(a[2], "Puncte miscandu-se pe ecran");
   a[3][0]= '\0';
   strcat(a[3], "Incheierea programului");
   m= 1;
   do
   {
      m= meniu(x, y, a, n, cul_chenar, cul_cursor, cul_text, cul_back, cul_high, m);
      switch(m)
      {
	 case 1:
	 {
	    initgraph(&gdriver, &gmode, "c:\\bc31\\bgi");
	    ii= 0;
	    jj= 0;
	    outtextxy(200, getmaxy()-10, "Apasati 'ESC' pentru terminare.");
	    for (i= 1; i <= getmaxy()-20; i++)
	       if (i%2 == 0)
		  for (j= getmaxx(); j >= 1; j--)
		  {
		     putpixel(jj, ii, 0);
		     ii= i;
		     jj= j;
		     putpixel(jj, ii, 2);
		     delay(1);
		     if (kbhit())
		     {
			j= 0;
			i= getmaxy()+1;
		     }
		  }
	       else
		  for (j= 1; j <= getmaxx(); j++)
		  {
		     putpixel(jj, ii, 0);
		     ii= i;
		     jj= j;
		     putpixel(jj, ii, 2);
		     delay(1);
		     if (kbhit())
		     {
			j= getmaxx()+1;
			i= getmaxy()+1;
		     }
		  }
	    closegraph();
	    break;
	 }
	 case 2:
	 {
	    initgraph(&gdriver, &gmode, "c:\\bc31\\bgi");
	    outtextxy(200, getmaxy()-10, "Apasati 'ESC' pentru terminare.");
	    for (i= 1; i <= getmaxy()-20; i++)
	    {
	       b[i]= 2-i;
	       iii[i]= 0;
	       s[i]= 1;
	    }
	    do
	    {
	       for (i= 1; i <= getmaxy()-20; i+= 2)
	       {
		  putpixel(iii[i], i, 0);
		  iii[i]= b[i];
		  putpixel(b[i], i, 2);
		  b[i]+= s[i];
		  if (b[i] == getmaxx()+1)
		     s[i]= -1;
		  if (b[i] == 0)
                     s[i]= 1;
		  //delay(0);
	       }
	    }
	    while (!kbhit());
	    closegraph();
	    break;
	 }
      }
   }
   while (m != 3);

   return 0;
}
