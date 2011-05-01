#include <stdio.h>
#include <conio.h>
#include <stdlib.h>
#include <graphics.h>

FILE *f;
int i, j, x1[101], y1[101], x2[101], y2[101], x;
float a1, a2, i1, i2, j1;

main()
{
   int drv= DETECT, mod_g;

   if ((f= fopen("gr.in", "r")) == NULL)
   {
      printf("Eroare la deschiderea fisierului !");
      getch();
      return 0;
   }
   i= 0;
   while (!feof(f))
   {
      i++;
      fscanf(f, "%i %i %i %i\n", &x1[i], &y1[i], &x2[i], &y2[i]);
   }
   fclose(f);
   initgraph(&drv, &mod_g, "c:\\bc31\\bgi");
   setbkcolor(WHITE);
   cleardevice();
   setcolor(YELLOW);
   setwritemode(XOR_PUT);
   for (j= 1; j <= i; j++)
   {
      if (x1[j] != x2[j])
      {
	 a1= (float(y2[j]-y1[j]))/(float(x2[j]-x1[j]));
	 a2= (float(x1[j])*float(y2[j])-float(y1[j])*float(x2[j]))/(x2[j]-x1[j]);
	 i1= -float(a2);
	 i2= (float(a1)*float(getmaxx()))-float(a2);
	 for (x= 0; x <= getmaxx(); x++)
	 {
	    j1= (float(a1)*float(x))-float(a2);
	    line(x, j1, x, 0);
	 }
      }
      else
	 for (x= 0; x <= getmaxy(); x++)
	    line(x1[j], x, getmaxx(), x);
   }


   getch();
   return 0;
}