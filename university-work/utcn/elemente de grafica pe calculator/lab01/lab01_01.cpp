/*
 * Programul deplaseaza o imagine pe ecran la comanda utilizatorului.
 * (varianta cu setwritemode(XOR_PUT))
 *
 * autor: Bogdan DUMITRIU
 * grupa: 3231
 * data:  08.10.2001
 */

#include <graphics.h>
#include <stdio.h>
#include <conio.h>
#include <stdlib.h>

/*
 * Pasul cu care se va face deplasarea stanga/dreapta, sus/jos a imaginii.
 */
#define STEP 10

/*
 * Calea catre directorul ce contine Borland Graphical Interface.
 */
#define BGI "C:\\BC31\\BGI"

/*
 * Procedura deseneaza o imagine predefinita cu coltul din stanga sus in
 * coordonatele (x,y).
 */
void draw_picture(int x, int y)
{
	setcolor(RED);
	setlinestyle(SOLID_LINE, 1, 1);
	rectangle(x, y, x+100, y+50);
	setcolor(CYAN);
	setlinestyle(DOTTED_LINE, 1, 2);
	line(x+5, y+10, x+50, y+10);
	setcolor(LIGHTGREEN);
	setlinestyle(DASHED_LINE, 1, 3);
	line(x+130, y+50, x+100, y+10);
	setcolor(BLUE);
	setlinestyle(CENTER_LINE, 1, 4);
	rectangle(x+30, y+30, x+80, y+80);
}

/*
 * Programul principal care gestioneaza miscarea imaginii pe ecran.
 */
void main()
{
	// parametrii necesari pentru initializarea modului grafic
	int gd = DETECT, gm;
	char ch;
	// flag in functie de care se face iesirea din program
	int gata = 0;
	// coordonatele initiale ale imaginii
	int x = 10, y = 10;

	// initializarea modului grafic
	initgraph(&gd, &gm, BGI);

	// testarea initializarii corecte
	if (graphresult() != grOk)
	{
		printf("Nu s-a reusit initializarea modului grafic.");
		getch();
		exit(EXIT_FAILURE);
	}

	// setarea modului de desenare la XOR_PUT pentru a putea sterge o imagine
	// de pe ecran printr-o desenare dubla
	setwritemode(XOR_PUT);

	// desenarea imaginii initiale pe ecran
	draw_picture(x, y);

	// preluarea si executia comenzilor utilizator
	while (!gata)
	{
		// citirea tastei apasate de utilizator
		ch = getch();
		if (ch == 0)
		{
			ch = getch();
			switch (ch)
			{
				// deplasare in sus a imaginii
				case 72:
					// testare a incadrarii in ecran
					if ((y - STEP) >= 0)
					{
						draw_picture(x, y);
						y = y - STEP;
						draw_picture(x, y);
					}
					break;
				// deplasare in jos a imaginii
				case 80:
					// testare a incadrarii in ecran
					if ((y + 80 + STEP) <= getmaxy())
					{
						draw_picture(x, y);
						y = y + STEP;
						draw_picture(x, y);
					}
					break;
				// deplasare in stanga a imaginii
				case 75:
					// testare a incadrarii in ecran
					if ((x - STEP) >= 0)
					{
						draw_picture(x, y);
						x = x - STEP;
						draw_picture(x, y);
					}
					break;
				// deplasare in dreapta a imaginii
				case 77:
					// testare a incadrarii in ecran
					if ((x + 130 + STEP) <= getmaxx())
					{
						draw_picture(x, y);
						x = x + STEP;
						draw_picture(x, y);
					}
					break;
			}
		}
		else
			// setarea flagului de terminare in cazul apasarii tastei "ESC"
			if (ch == 27)
				gata = 1;
	}

	// iesirea din modul grafic
	closegraph();
}