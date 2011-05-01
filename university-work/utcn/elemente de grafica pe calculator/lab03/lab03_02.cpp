/*
 * Programul genereaza aleator un numar de segmente pe ecran dupa care
 * permite deplasarea unui dreptunghi peste ele. In momentul in care
 * se apasa tasta ENTER se sterge de pe ecran tot ceea ce nu se afla
 * in dreptunghi. Pentru aceasta se foloseste de metoda bisectiei.
 *
 * Autor: Bogdan DUMITRIU
 * Grupa: 3231
 * Data:  27.10.2001
 */

#include <stdio.h>
#include <conio.h>
#include <alloc.h>
#include <stdlib.h>
#include <graphics.h>
#include <math.h>

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
 * Coordonatele initiale ale coltului din stanga sus al dreptungiului
 * selector.
 */
int x = 0;
int y = 0;

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
 * Functia intoarce 1 daca segmentul dintre punctele (x1,y1) si (x2,y2)
 * poate fi respins simplu (ambele puncte se afla in exteriorul
 * dreptunghiului cu coltul din stanga sus in (x,y) si cel din
 * dreapta jos in (x+LENGTH,y+WIDTH), de aceeasi parte a acestuia, unde
 * x si y sunt variabile globale, iar LENGTH si WIDTH sunt macrouri).
 */
int respingere_simpla(int x1, int y1, int x2, int y2)
{
	int res = 0;
	if ((x1 <= x+1) && (x2 <= x+1))
		res = 1;
	if ((x1 >= x+LENGTH-1) && (x2 >= x+LENGTH-1))
		res = 1;
	if ((y1 <= y+1) && (y2 <= y+1))
		res = 1;
	if ((y1 >= y+WIDTH-1) && (y2 >= y+WIDTH-1))
		res = 1;

	return res;
}

/*
 * Functia intoarce 1 daca segmentul dintre punctele (x1,y1) si (x2,y2)
 * poate fi acceptat simplu (ambele puncte se afla in interiorul
 * dreptunghiului cu coltul din stanga sus in (x,y) si cel din
 * dreapta jos in (x+LENGTH,y+WIDTH), unde x si y sunt variabile
 * globale, iar LENGTH si WIDTH sunt macrouri).
 */
int acceptare_simpla(int x1, int y1, int x2, int y2)
{
	int res = 0;
	if ((x1 > x) && (y1 > y) && (x1 < x+LENGTH) && (y1 < y+WIDTH)
		&& (x2 > x) && (y2 > y) && (x2 < x+LENGTH) && (y2 < y+WIDTH))
	{
		res = 1;
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
 * Calculeaza si intoarce valoarea medie dintre punctele x1 si x2
 */
double calcul_med(double x1, double x2)
{
	double xmed;

	xmed = (double) (x1+x2);
	xmed = (double) xmed/2;

	return xmed;
}

double min(double x1, double x2, double x3, double x4)
{
	double minim = x1;
	if (minim > x2)
		minim = x2;
	if (minim > x3)
		minim = x3;
	if (minim > x4)
		minim = x4;

	return minim;
}

double max(double x1, double x2, double x3, double x4)
{
	double maxim = x1;
	if (maxim < x2)
		maxim = x2;
	if (maxim < x3)
		maxim = x3;
	if (maxim < x4)
		maxim = x4;

	return maxim;
}

/*
 * Functie folosita in cadrul metodei divide et impera utilizata in
 * implementarea metodei bisectiei. Scopul ei e de a construi din
 * doua segmente unul singur.
 */
void combina(int acc1, int acc2, double x1, double y1,
	double x2, double y2, double x3, double y3, double x4, double y4,
	double *xx1, double *yy1, double *xx2, double *yy2)
{
	if ((acc1) && (acc2))
	{
		(*xx1) = (double) min(x1, x2, x3, x4);
		(*xx2) = (double) max(x1, x2, x3, x4);
		double aux = *xx1;
		if (aux == x1)
		{
			(*yy1) = (double) y1;
		}
		if (aux == x2)
		{
			(*yy1) = (double) y2;
		}
		if (aux == x3)
		{
			(*yy1) = (double) y3;
		}
		if (aux == x4)
		{
			(*yy1) = (double) y4;
		}
		aux = *xx2;
		if (aux == x1)
		{
			(*yy2) = (double) y1;
		}
		if (aux == x2)
		{
			(*yy2) = (double) y2;
		}
		if (aux == x3)
		{
			(*yy2) = (double) y3;
		}
		if (aux == x4)
		{
			(*yy2) = (double) y4;
		}

	}
	if ((acc1) && (!acc2))
	{
		(*xx1) = (double) x1;
		(*xx2) = (double) x2;
		(*yy1) = (double) y1;
		(*yy2) = (double) y2;
	}
	if ((!acc1) && (acc2))
	{
		(*xx1) = (double) x3;
		(*xx2) = (double) x4;
		(*yy1) = (double) y3;
		(*yy2) = (double) y4;
	}
}

/*
 * Implementarea metodei bisectiei. Metoda are ca scop determinarea
 * acelei parti a segmentului dintre punctele (x1,y1) si (x2,y2)
 * care se afla in interiorul dreptunghiului cu coltul din stanga
 * sus in (x,y) si coltul din dreapta jos in (x+LENGTH,y+LENGTH),
 * unde x si y sunt variabile globale, iar LENGTH si WIDTH sunt
 * macrouri. Coordonatele respectivei parti se vor gasi dupa executia
 * functiei in variabilele (xin1,yin1) si (xin2,yin2). Functia
 * intoarce 0 daca nici o parte a segmentului nu se afla in
 * dreptunghi si 1 in caz contrar.
 */
int bisectie_(double x1, double y1, double x2, double y2,
	double *xin1, double *yin1, double *xin2, double *yin2)
{
	int accept1, accept2;
	double xx1, xx2, xx3, xx4, yy1, yy2, yy3, yy4;
	double xmed, ymed;
	double val;

	if (acceptare_simpla(round(x1), round(y1), round(x2), round(y2)))
	{
		(*xin1) = (double) x1;
		(*yin1) = (double) y1;
		(*xin2) = (double) x2;
		(*yin2) = (double) y2;
		return 1;
	}
	if (respingere_simpla(round(x1), round(y1), round(x2), round(y2)))
	{
		(*xin1) = -1;
		(*yin1) = -1;
		(*xin2) = -1;
		(*yin2) = -1;
		return 0;
	}

	xmed = (double) calcul_med(x1, x2);
	ymed = (double) calcul_med(y1, y2);

	accept1 = bisectie_(x1, y1, xmed, ymed, &xx1, &yy1, &xx2, &yy2);
	accept2 = bisectie_(x2, y2, xmed, ymed, &xx3, &yy3, &xx4, &yy4);
	combina(accept1, accept2, xx1, yy1, xx2, yy2, xx3, yy3, xx4, yy4,
		xin1, yin1, xin2, yin2);
	return 1;
}

/*
 * Functie intermediara, cu rolul de a transforma parametrii de intrare
 * din numere intregi in numere reale.
 */
void bisectie(int x1, int y1, int x2, int y2,
	int *xin1, int *yin1, int *xin2, int *yin2)
{
	double xx1 = (double) x1;
	double yy1 = (double) y1;
	double xx2 = (double) x2;
	double yy2 = (double) y2;
	double xxin1, yyin1, xxin2, yyin2;

	bisectie_(xx1, yy1, xx2, yy2, &xxin1, &yyin1, &xxin2, &yyin2);

	(*xin1) = round(xxin1);
	(*yin1) = round(yyin1);
	(*xin2) = round(xxin2);
	(*yin2) = round(yyin2);
}

void main()
{
	int x1[NR_OF_SEGMENTS], y1[NR_OF_SEGMENTS];
	int x2[NR_OF_SEGMENTS], y2[NR_OF_SEGMENTS];
	int color[NR_OF_SEGMENTS];
	char ch;
	int gata = 0;

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

	// Se aplica pe fiecare segment metoda bisectiei.
	for (i = 0; i < NR_OF_SEGMENTS; i++)
	{
		bisectie(x1[i], y1[i], x2[i], y2[i], &x1[i], &y1[i], &x2[i], &y2[i]);
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