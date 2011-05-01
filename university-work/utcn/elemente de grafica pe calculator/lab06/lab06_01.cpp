/*
 * Programul implementeaza algoritmul lui Bresenham pentru trasarea
 * unui segment de dreapta.
 *
 * Autor: Bogdan DUMITRIU
 * Grupa: 3231
 * Data:  12.11.2001
 */

#include <graphics.h>
#include <stdio.h>
#include <conio.h>
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

/*
 * Algoritmul (lui Bresenham) efectiv. Parametrii x1, y1, x2, y2
 * reprezinta coordonatele ecran ale capetelor segmentului.
 */
void draw_line(int x1, int y1, int x2, int y2)
{
	int x, y, xc, yc;

	int dx = abs(x2-x1);
	int dy = abs(y2-y1);

	int caz = 0;

	if (((x1-x2) <= 0) && ((y1-y2) <= 0))
		caz = 1;
	if (((x1-x2) > 0) && ((y1-y2) > 0))
		caz = 1;

	if (dx >= dy)
	{
		int d = 2*dy-dx;
		int inc1 = 2*dy;
		int inc2 = 2*(dy-dx);
		if (x1 > x2)
		{
			x = x2;
			y = y2;
			xc = x1;
		}
		else
		{
			x = x1;
			y = y1;
			xc = x2;
		}
		while (x < xc)
		{
			putpixel(x, y, 15);
			x++;
			if (d < 0)
				d = d+inc1;
			else
			{
				if (caz)
					y++;
				else
					y--;
				d = d+inc2;
			}
		}
	}
	else
	{
		int d = 2*dx-dy;
		int inc1 = 2*dx;
		int inc2 = 2*(dx-dy);
		if (y1 > y2)
		{
			x = x2;
			y = y2;
			yc = y1;
		}
		else
		{
			x = x1;
			y = y1;
			yc = y2;
		}
		while (y < yc)
		{
			putpixel(x, y, 15);
			y++;
			if (d < 0)
				d = d+inc1;
			else
			{
				if (caz)
					x++;
				else
					x--;
				d = d+inc2;
			}
		}

	}
}

/*
 * Se apeleaza algoritmul pentru cele 4 cazuri diferite
 * ce pot apare.
 */
void main()
{
	initialize_graphic_mode();
	draw_line(100, 105, 420, 200);	// cazul 1
	draw_line(100, 200, 420, 105);	// cazul 2
	draw_line(105, 100, 200, 420);	// cazul 3
	draw_line(200, 100, 105, 420);	// cazul 4
	getch();
	closegraph();
}