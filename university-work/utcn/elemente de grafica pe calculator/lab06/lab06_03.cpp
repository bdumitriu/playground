/*
 * Programul implementeaza un algoritm pentru trasarea unei elipse.
 *
 * Autor: Bogdan DUMITRIU
 * Grupa: 3231
 * Data:  01.12.2001
 */

#include <graphics.h>
#include <stdio.h>
#include <conio.h>
#include <stdlib.h>
#include <math.h>

/*
 * Calea catre directorul ce contine Borland Graphical Interface.
 */
#define BGI "C:\\BC31\\BGI"

/*
 * Initializeaza modul grafic folosing macroul BGI.
 */
void initialize_graphic_mode()
{
	int gd = DETECT, gm, err;

	initgraph(&gd, &gm, BGI);
	if ((err = graphresult()) != grOk)
	{
		printf("Eroare: %s", grapherrormsg(err));
		getch();
		exit(1);
	}
}

int min(double m1, double m2, double m3)
{
	if (m2 <= m1)
		if (m2 <= m3)
			return 2;
		else
			return 3;
	else
		if (m1 <= m3)
			return 1;
		else
			return 3;
}

/*
 * Algoritmul efectiv. Parametrii rx, ry, cx si cy reprezinta
 * raza pe x si raza pe y, respectiv coordonatele ecran ale centrului
 * elipsei.
 */
void draw_ellipse(int rx, int ry, int cx, int cy)
{
	int x = rx;
	int y = 0;

	do
	{
		putpixel(cx+x, cy+y, YELLOW);
		putpixel(cx-x, cy+y, RED);
		putpixel(cx-x, cy-y, BLUE);
		putpixel(cx+x, cy-y, CYAN);

		double t1 = (double) ((double) x*x)/((double) rx*rx);
		double t2 = (double) ((double) (x-1)*(x-1))/((double) rx*rx);
		double t3 = (double) ((double) y*y)/((double) ry*ry);
		double t4 = (double) ((double) (y-1)*(y-1))/((double) ry*ry);

		double m1 = fabs(t2+t3-1);
		double m2 = fabs(t2+t4-1);
		double m3 = fabs(t1+t4-1);

		switch (min(m1, m2, m3))
		{
			case 1:
				x--;
				break;
			case 2:
				x--;
				y--;
				break;
			case 3:
				y--;
				break;
		}
	}
	while (x >= 0);
}

void main()
{
	initialize_graphic_mode();
	draw_ellipse(215, 33, 287, 158);
	getch();
	ellipse(287, 158, 0, 360, 215, 33);
	getch();
	closegraph();
}