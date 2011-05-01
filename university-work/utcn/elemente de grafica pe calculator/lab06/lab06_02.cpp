/*
 * Programul implementeaza algoritmul lui Bresenham pentru trasarea
 * unui cerc.
 *
 * Autor: Bogdan DUMITRIU
 * Grupa: 3231
 * Data:  19.11.2001
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

/*
 * Algoritmul (lui Bresenham) efectiv. Parametrii R, cx si cy
 * reprezinta raza, respectiv coordonatele ecran ale centrului
 * cercului.
 */
void draw_circle(int cx, int cy, int R)
{
	int x = 0;
	int y = R;
	int d1, d2;
	int D = 2-2*R;

	do
	{
		putpixel(cx+x, cy+y, YELLOW);
		putpixel(cx-x, cy+y, RED);
		putpixel(cx-x, cy-y, BLUE);
		putpixel(cx+x, cy-y, CYAN);

		d1 = 2*D+2*y-1;
		d2 = 2*D-2*x-1;

		if ((D < 0) && (d1 <= 0))
		{
			x++;
			D = D+2*x+1;
		}
		else
			if (((D < 0) && (d1 > 0)) || ((D > 0) && (d2 < 0)) || (D == 0))
			{
				x++;
				y--;
				D = D+2*x-2*y+2;
			}
			else
				if ((D > 0) && (d2 >= 0))
				{
					y--;
					D = D-2*y+1;
				}
	}
	while (y >= 0);
}

void main()
{
	initialize_graphic_mode();
	draw_circle(200, 100, 50);
	getch();
	circle(200, 100, 50);
	getch();
	closegraph();
}