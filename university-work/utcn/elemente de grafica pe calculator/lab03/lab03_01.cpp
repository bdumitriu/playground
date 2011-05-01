/*
 * Programul genereaza aleator un numar de segmente pe ecran dupa care
 * permite deplasarea unui dreptunghi peste ele. In momentul in care
 * se apasa tasta ENTER se sterge de pe ecran tot ceea ce nu se afla
 * in dreptunghi. Pentru aceasta se foloseste algoritmul Cohen
 * Sutherland.
 *
 * Autor: Bogdan DUMITRIU
 * Grupa: 3231
 * Data:  26.10.2001
 */

#include <stdio.h>
#include <conio.h>
#include <alloc.h>
#include <stdlib.h>
#include <graphics.h>

/*
 * Numarul de segmente ce vor fi generate.
 */
#define NR_OF_SEGMENTS 20

/*
 * Lungimea dreptunghiului.
 */
#define LENGTH 200

/*
 * Latimea dreptunghiului.
 */
#define WIDTH 150

/*
 * Pasul cu care se va misca dreptunghiul stanga/dreapta, sus/jos.
 */
#define STEP 10

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
 * Functia intoarce codul punctului (x,y) relativ la dreptunghiul cu
 * coltul din stanga sus in (x1,y1) si coltul din dreapta jos in
 * (x2,y2) (vezi descrierea algoritmului Cohen-Sutherland (in carti
 * de teoria graficii pe calculator) pentru a afla modul de
 * codificare folosit).
 */
int *code(double x, double y, int x1, int y1, int x2, int y2)
{
	int *a;

	a = (int *) malloc(4*sizeof(int));

	if (y < y1)
	{
		a[0] = 1;
	}
	else
	{
		a[0] = 0;
	}
	if (y > y2)
	{
		a[1] = 1;
	}
	else
	{
		a[1] = 0;
	}
	if (x > x2)
	{
		a[2] = 1;
	}
	else
	{
		a[2] = 0;
	}
	if (x < x1)
	{
		a[3] = 1;
	}
	else
	{
		a[3] = 0;
	}

	return a;
}

/*
 * Functia intoarce 1 daca segmentul dintre punctele cu codurile a si b
 * poate fi respins simplu (ambele puncte se afla in exteriorul
 * dreptunghiului, de aceeasi parte a acestuia).
 */
int simple_rejection(int *a, int *b)
{
	int res = 0;
	for (int i = 0; i < 4; i++)
		if ((a[i] == 1) && (b[i] == 1))
			res = 1;

	return res;
}

/*
 * Functia intoarce 1 daca segmentul dintre punctele cu codurile a si b
 * poate fi acceptat simplu (ambele puncte se afla in interiorul
 * dreptunghiului).
 */
int simple_acception(int *a, int *b)
{
	int res = 1;
	for (int i = 0; i < 4; i++)
		if ((a[i] != 0) || (b[i] != 0))
			res = 0;

	return res;
}

/*
 * Functia intoarce 1 daca toate cele 4 elemente ale vectorului
 * c au valoarea 0. Acest lucru e echivalent cu faptul ca punctul
 * cu codul cod se afla in interiorul dreptunghiului (vezi descrierea
 * algoritmului Cohen-Sutherland (in carti de teoria graficii pe
 * calculator) pentru a afla modul de codificare folosit).
 */
int inside(int *cod)
{
	int res = 1;
	for (int i = 0; i < 4; i++)
	{
		if (cod[i] == 1)
			res = 0;
	}

	return res;
}

/*
 * Functia intoarce valoarea rotunjita a lui x.
 */
int round(double x)
{
	if (x >= 0)
	{
		if ((x - ((int) x)) < 0.5)
			return ((int) x);
		else
			return ((int) x)+1;
	}
	else
	{
		if ((((int) x) - x) < 0.5)
			return ((int) x);
		else
			return ((int) x)-1;
	}
}

/*
 * Implementarea algoritmului lui Cohen-Sutherland. Acest algoritm
 * calculeaza cat din segmentul dintre punctele (x_1,y_1) si (x_2,y_2)
 * se afla in zona dreptunghiulara cu coltul din stanga sus in
 * (Xmin,Ymin) si coltul din dreapta jos in (Xmax,Ymax). Functia intoarce
 * valoarea 0 daca segmentul este respins total (nici o parte a sa
 * nu se afla in dreptunghi) si 1 daca cel putin o parte a segmentului
 * se afla in dreptunghi. In acest al doilea caz partea din segment
 * care se afla in dreptunghi va fi intre punctele (xin1,yin1) si
 * (xin2,yin2).
 */
int Cohen_Sutherland_algorithm(
	int x_1, int y_1, int x_2, int y_2,
	int Xmin, int Ymin, int Xmax, int Ymax,
	int *xin1, int *yin1, int *xin2, int *yin2)
{
	int *cod1, *cod2;
	int gata = 0;
	int accept = 0;
	int resping = 0;
	double x1 = x_1;
	double x2 = x_2;
	double y1 = y_1;
	double y2 = y_2;
	double val;

	do
	{
		cod1 = code(x1, y1, Xmin, Ymin, Xmax, Ymax);
		cod2 = code(x2, y2, Xmin, Ymin, Xmax, Ymax);
		if (simple_rejection(cod1, cod2))
		{
			gata = 1;
			resping = 1;
		}
		else
		{
			if (simple_acception(cod1, cod2))
			{
				gata = 1;
				accept = 1;
			}
			else
			{
				if (inside(cod1))
				{
					double aux;
					aux = x1;
					x1 = x2;
					x2 = aux;
					aux = y1;
					y1 = y2;
					y2 = aux;
					for (int i = 0; i < 4; i++)
					{
						aux = cod1[i];
						cod1[i] = cod2[i];
						cod2[i] = aux;
					}
				}
				if ((cod1[0] == 1) && (y1 != y2))
				{
					val = (double) (Ymin-y1);
					val = (double) (val*(x2-x1));
					val = (double) (val/(y2-y1));
					val = (double) (x1+val);
					x1 = (double) val;
					y1 = (double) Ymin;
				}
				else
				{
					if ((cod1[1] == 1) && (y1 != y2))
					{
						val = (double) (Ymax-y1);
						val = (double) (val*(x2-x1));
						val = (double) (val/(y2-y1));
						val = (double) (x1+val);
						x1 = (double) val;
						y1 = (double) Ymax;
					}
					else
					{
						if ((cod1[2] == 1) && (x1 != x2))
						{
							val = (double) (Xmax-x1);
							val = (double) (val*(y2-y1));
							val = (double) (val/(x2-x1));
							val = (double) (y1+val);
							y1 = (double) val;
							x1 = (double) Xmax;
						}
						else
						{
							if ((cod1[3] == 1) && (x1 != x2))
							{
								val = (double) (Xmin-x1);
								val = (double) (val*(y2-y1));
								val = (double) (val/(x2-x1));
								val = (double) (val+y1);
								y1 = (double) val;
								x1 = (double) Xmin;
							}
						}
					}
				}
			}
		}
	}
	while (!gata);

	if (resping)
	{
		(*xin1) = -1;
		(*yin1) = -1;
		(*xin2) = -1;
		(*yin2) = -1;
	}
	else
	{
		if (accept)
		{
			(*xin1) = round(x1);
			(*yin1) = round(y1);
			(*xin2) = round(x2);
			(*yin2) = round(y2);
		}
	}

	return accept;
}

void main()
{
	int x1[NR_OF_SEGMENTS], y1[NR_OF_SEGMENTS];
	int x2[NR_OF_SEGMENTS], y2[NR_OF_SEGMENTS];
	int color[NR_OF_SEGMENTS];
	char ch;
	int gata = 0;
	int x = 0;
	int y = 0;
    
	initialize_graphic_mode();

	setwritemode(XOR_PUT);

	// Se genereaza NR_OF_SEGMENTS segmente aleatoare.
	randomize();
	for (int i = 0; i < NR_OF_SEGMENTS; i++)
	{
		x1[i] = random(640);
		y1[i] = random(480);
		x2[i] = random(640);
		y2[i] = random(480);
		do
		{
			color[i] = random(16);
		}
		while (color[i] == 0);
	}

	// Se deseneaza segemntele generate anterior pe ecran
	for (i = 0; i < NR_OF_SEGMENTS; i++)
	{
		setcolor(color[i]);
		line(x1[i], y1[i], x2[i], y2[i]);
	}

	// Se deseneaza dreptunghiul selector
	setcolor(MAGENTA);
	rectangle(x, y, LENGTH, WIDTH);

	// Se permite miscarea dreptunghiului sus/jos, stanga/dreapta pana
	// la apasarea tastei ENTER.
	while (!gata)
	{
		// citirea tastei apasate de utilizator
		ch = getch();
		if (ch == 0)
		{
			ch = getch();
			switch (ch)
			{
				// deplasare in sus a dreptunghiului
				case 72:
					if ((y-STEP) >= 0)
					{
						rectangle(x, y, x+LENGTH, y+WIDTH);
						y = y-STEP;
						rectangle(x, y, x+LENGTH, y+WIDTH);
					}
					break;
				// deplasare in jos a dreptunghiului
				case 80:
					if ((y+WIDTH+STEP) <= getmaxy())
					{
						rectangle(x, y, x+LENGTH, y+WIDTH);
						y = y+STEP;
						rectangle(x, y, x+LENGTH, y+WIDTH);
					}
					break;
				// deplasare in stanga a dreptunghiului
				case 75:
					if ((x-STEP) >= 0)
					{
						rectangle(x, y, x+LENGTH, y+WIDTH);
						x = x-STEP;
						rectangle(x, y, x+LENGTH, y+WIDTH);
					}
					break;
				// deplasare in dreapta a dreptunghiului
				case 77:
					if ((x+LENGTH+STEP) <= getmaxx())
					{
						rectangle(x, y, x+LENGTH, y+WIDTH);
						x = x+STEP;
						rectangle(x, y, x+LENGTH, y+WIDTH);
					}
					break;
			}
		}
		else
		{
			if (ch == 13)
			{
				gata = 1;
			}
		}
	}

	// Se sterg toate segmentele de pe ecran.
	for (i = 0; i < NR_OF_SEGMENTS; i++)
	{
		setcolor(color[i]);
		line(x1[i], y1[i], x2[i], y2[i]);
	}

	// Se aplica pe fiecare segment algoritmul Cohen-Sutherland.
	for (i = 0; i < NR_OF_SEGMENTS; i++)
	{
		Cohen_Sutherland_algorithm(x1[i], y1[i], x2[i], y2[i],
			x+1, y+1, x+LENGTH-1, y+WIDTH-1, &x1[i], &y1[i], &x2[i], &y2[i]);
	}

	// Se redeseneaza doar acele parti din segmente care se afla
	// in dreptunghi.
	for (i = 0; i < NR_OF_SEGMENTS; i++)
	{
		setcolor(color[i]);
		line(x1[i], y1[i], x2[i], y2[i]);
	}

	getch();
}