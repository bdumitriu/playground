/*
 * Programul afiseaza doua ferestre cu un grafic de functie. Se poate face
 * zoom in, zoom out, deplasare stanga-dreapta si sus-jos a graficului.
 * Intr-una din ferestre se va vedea un dreptunghi deasupra graficului.
 * Continutul dreptunghiului este ceea ce e afisat in a doua fereastra.
 *
 * Autor: Bogdan DUMITRIU
 * Grupa: 3231
 * Data:  15.10.2001
 */

#include <stdlib.h>
#include <conio.h>
#include <stdio.h>
#include <math.h>
#include <values.h>
#include <graphics.h>
#include <alloc.h>

/*
 * Limita inferioara a intervalului pe care se calculeaza functia.
 */
#define a -2*M_PI

/*
 * Limita superioara a intervalului pe care se calculeaza functia.
 */
#define b 2*M_PI

/*
 * Calea catre directorul ce contine Borland Graphical Interface.
 */
#define BGI "C:\\BC31\\BGI"

/*
 * Numarul de puncte ale graficului ce vor fi calculate.
 */
#define REF 2000

/*
 * Numarul de pixeli cu care se face deplasarea stanga/dreapta, sus/jos.
 */
#define MOVE_STEP 10

/*
 * Numarul de pixeli care se adauga/scad in/din toate partile la zoom
 * out/in.
 */
#define ZOOM_STEP 10

/*
 * (x1,y1) - coltul din dreapta sus al dreptunghiului selector
 * (x2,y2) - coltul din stanga jos al dreptunghiului selector
 */
int x1 = 0, y1 = 0, x2 = 316, y2 = 316;

/*
 * Valorile minima si maxima a functiei.
 */
double minf, maxf;

/*
 * Coordonatele graficului scalate pentru zona de afisare din stanga.
 */
double x[REF+1], y[REF+1];

/*
 * Functia de afisat.
 */
double f(double x)
{
	return sin(x);
}

/*
 * Intoarce valoarea maxima a functiei f luand esantioane din
 * ((b)-(a))/REF in ((b)-(a))/REF (vezi la inceputul programului
 * definitia pentru REF, a si b).
 */
double compute_maxf()
{
	double step = (double) ((b)-(a))/REF;
	double max = -MAXDOUBLE;

	for (int i = 0; i <= REF; i++)
	{
		if (max < f(a+i*step))
			max = f(a+i*step);
	}

	return max;
}

/*
 * Intoarce valoarea minima a functiei f luand esantioane din
 * ((b)-(a))/REF in ((b)-(a))/REF (vezi la inceputul programului
 * definitia pentru REF, a si b).
 */
double compute_minf()
{
	double step = (double) ((b)-(a))/REF;
	double min = MAXDOUBLE;

	for (int i = 0; i <= REF; i++)
	{
		if (min > f(a+i*step))
			min = f(a+i*step);
	}

	return min;
}

/*
 * Intoarce acel x pentru care valoarea (in modul) a functiei f e
 * minima.
 */
double zerof()
{
	double step = (double) ((b)-(a))/REF;
	double zero = MAXDOUBLE;
	double retvalue = 0;

	for (int i = 0; i <= REF; i++)
	{
		if (zero > abs(f(a+i*step)))
			retvalue = a+i*step;
	}

	return retvalue;
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
 * Setarea vederii active.
 */
void set(int i)
{
	switch (i)
	{
		case 0:
			setviewport(0, 80, 318, 398, 1);
			break;
		case 1:
			setviewport(321, 80, 639, 398, 1);
			break;
		case 2:
			setviewport(1, 81, 317, 397, 1);
			break;
		case 3:
			setviewport(322, 81, 638, 397, 1);
			break;
		case 4:
			setviewport(10, 410, 630, 475, 1);
			break;
	}
}

/*
 * Desenarea celor doua dreptunghiuri in care vom face afisarea.
 */
void draw_borders()
{
	setcolor(LIGHTGRAY);
	set(0);
	rectangle(0, 0, 318, 318);
	set(1);
	rectangle(0, 0, 318, 318);
}

/*
 * Afiseaza pe ecran comenzile disponibile.
 */
void print_usage_text()
{
	set(4);
	setcolor(RED);
	outtext("Comenzi disponibile: ");
	outtextxy(3, 15, "-");
	outtextxy(6, 15, ">");
	setcolor(LIGHTBLUE);
	outtextxy(20, 15, "deplasare dreapta");
	setcolor(RED);
	outtextxy(3, 30, "<");
	outtextxy(6, 30, "-");
	setcolor(LIGHTBLUE);
	outtextxy(20, 30, "deplasare stanga");
	setcolor(RED);
	outtextxy(200, 12, "^");
	line(203, 15, 203, 21);
	setcolor(LIGHTBLUE);
	outtextxy(216, 15, "deplasare in sus");
	setcolor(RED);
	line(203, 30, 203, 36);
	putpixel(200, 36, RED);
	putpixel(201, 36, RED);
	putpixel(205, 36, RED);
	putpixel(206, 36, RED);
	putpixel(201, 37, RED);
	putpixel(202, 37, RED);
	putpixel(204, 37, RED);
	putpixel(205, 37, RED);
	putpixel(202, 38, RED);
	putpixel(203, 38, RED);
	putpixel(204, 38, RED);
	setcolor(LIGHTBLUE);
	outtextxy(216, 30, "deplasare in jos");
	setcolor(RED);
	outtextxy(400, 15, "+");
	setcolor(LIGHTBLUE);
	outtextxy(415, 15, "zoom in");
	setcolor(RED);
	outtextxy(400, 30, "-");
	setcolor(LIGHTBLUE);
	outtextxy(415, 30, "zoom out");
	setcolor(RED);
	outtextxy(520, 15, "ESC");
	setcolor(LIGHTBLUE);
	outtextxy(553, 15, "iesire");
}

/*
 * Calculul coordonatei x pentru zona de afisat.
 */
int virtualX(double x)
{
	double d = (double) 316/((b)-(a));
	d = (double) d*(x-(a));
	return round(d);
}

/*
 * Calculul coordonatei y pentru zona de afisat.
 */
int virtualY(double y)
{
	double d = (double) 316/(maxf-minf);
	d = (double) d*(y-minf);
	d = (double) 316-d;
	return round(d);
}

/*
 * Scalarea functiei reale la zona de afisat.
 */
void scale_function()
{
	double step = (double) ((b)-(a))/REF;

	for (int i = 0; i <= REF; i++)
	{
		x[i] = virtualX(a+i*step);
		y[i] = virtualY(f(a+i*step));
	}
}

/*
 * Desenarea graficului in zona din stanga a ecranului.
 */
void initialize_left_area()
{
	set(2);

	// desenarea graficului
	setcolor(LIGHTBLUE);
	moveto(x[0], y[0]);

	for (int i = 1; i <= REF; i++)
		lineto(x[i], y[i]);

	setcolor(LIGHTGRAY);
	// desenarea axelor daca sunt in figura
	if ((minf <= 0) && (maxf >= 0))
	{
		// axa Ox
		line(0, virtualY(f(zerof())), 316, virtualY(f(zerof())));
	}
	if ((a <= 0) && (b >= 0))
	{
		// axa Oy
		line(virtualX(0), 0, virtualX(0), 316);
	}

	setwritemode(XOR_PUT);
	setcolor(GREEN);
	rectangle(x1, y1, x2, y2);
}

/*
 * Functia care deplaseaza, mareste sau micsoreaza dreptunghiul de
 * selectie in functie de parametrul flag folosind macrourile
 * MOVE_STEP si ZOOM_STEP. Parametrul flag are urmatoarea
 * semnificatie:
 *  0: misca sus;
 *  1: misca jos;
 *  2: misca stanga;
 *  3: misca dreapta;
 *  4: zoom in;
 *  5: zoom out;
 *  alta valoare: nu se face nimic.
 */
void move_selector(int flag)
{
	int oldx1 = x1;
	int oldy1 = y1;
	int oldx2 = x2;
	int oldy2 = y2;

	switch(flag)
	{
		// misca in sus
		case 0:
			if ((y1-MOVE_STEP) >= 0)
			{
				y1 = y1-MOVE_STEP;
				y2 = y2-MOVE_STEP;
			}
			else
			{
				y2 = y2-y1;
				y1 = 0;
			}
			break;
		// misca in jos
		case 1:
			if ((y2+MOVE_STEP) <= 316)
			{
				y1 = y1+MOVE_STEP;
				y2 = y2+MOVE_STEP;
			}
			else
			{
				y1 = y1+(316-y2);
				y2 = 316;
			}
			break;
		// misca in stanga
		case 2:
			if ((x1-MOVE_STEP) >= 0)
			{
				x1 = x1-MOVE_STEP;
				x2 = x2-MOVE_STEP;
			}
			else
			{
				x2 = x2-x1;
				x1 = 0;
			}
			break;
		// misca in dreapta
		case 3:
			if ((x2+MOVE_STEP) <= 316)
			{
				x1 = x1+MOVE_STEP;
				x2 = x2+MOVE_STEP;
			}
			else
			{
				x1 = x1+(316-x2);
				x2 = 316;
			}
			break;
		// zoom in
		case 4:
			if (((x2-x1-2*ZOOM_STEP) >= 50) && ((y2-y1-2*ZOOM_STEP) >= 50))
			{
				x1 = x1+ZOOM_STEP;
				x2 = x2-ZOOM_STEP;
				y1 = y1+ZOOM_STEP;
				y2 = y2-ZOOM_STEP;
			}
			else
			{
				int maxzoom;
				if ((x2-x1) > (y2-y1))
				{
					maxzoom = (int) (((y2-y1)-50)/2);
				}
				else
				{
					maxzoom = (int) (((x2-x1)-50)/2);
				}
				x1 = x1+maxzoom;
				x2 = x2-maxzoom;
				y1 = y1+maxzoom;
				y2 = y2-maxzoom;
			}
			break;
		// zoom out
		case 5:
			if ((x1-ZOOM_STEP) < 0)
			{
				x1 = 0;
			}
			else
			{
				x1 = x1-ZOOM_STEP;
			}
			if ((x2+ZOOM_STEP) > 316)
			{
				x2 = 316;
			}
			else
			{
				x2 = x2+ZOOM_STEP;
			}
			if ((y1-ZOOM_STEP) < 0)
			{
				y1 = 0;
			}
			else
			{
				y1 = y1-ZOOM_STEP;
			}
			if ((y2+ZOOM_STEP) > 316)
			{
				y2 = 316;
			}
			else
			{
				y2 = y2+ZOOM_STEP;
			}
			break;
	}
	set(2);
	setwritemode(XOR_PUT);
	setcolor(GREEN);
	rectangle(oldx1, oldy1, oldx2, oldy2);
	rectangle(x1, y1, x2, y2);
}

/*
 * Scaleaza coordonata x de la cea din dreptunghiul selector la cea din
 * zona din dreapta.
 */
int newX(int x)
{
	double d = (double) 316/(x2-x1);
	d = (double) d*(x-x1);
	return round(d);
}

/*
 * Scaleaza coordonata y de la cea din dreptunghiul selector la cea din
 * zona din dreapta.
 */
int newY(int y)
{
	double d = (double) 316/(y2-y1);
	d = (double) d*(y-y1);
	return round(d);
}

/*
 * Redeseneaza zona din dreapta dupa ce utilizatorul a miscat sau a
 * modificat dreptunghiul in zona din stanga.
 */
void redraw_right_area()
{
	int restidx, pi;

	set(3);
	setwritemode(COPY_PUT);
	setcolor(LIGHTBLUE);
	clearviewport();
	for (int i = 0; i <= REF; i++)
	{
		if ((x[i] >= x1) && (x[i] <= x2) && (y[i] >= y1) && (y[i] <= y2))
		{
			moveto(newX(x[i]), newY(y[i]));
			pi = i;
			restidx = i+1;
			i = REF+1;
		}
	}
	for (i = restidx; i <= REF; i++)
	{
		if ((x[i] >= x1) && (x[i] <= x2) && (y[i] >= y1) && (y[i] <= y2))
		{
			if ((pi+1) == i)
			{
				lineto(newX(x[i]), newY(y[i]));
			}
			else
			{
				moveto(newX(x[i]), newY(y[i]));
			}
			pi = i;
		}
	}
}

/*
 * Preia comenzile utilizatorului si le interpreteaza.
 */
void get_user_input()
{
	char ch;
	int gata = 0;

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
					move_selector(0);
					redraw_right_area();
					break;
				// deplasare in jos a dreptunghiului
				case 80:
					move_selector(1);
					redraw_right_area();
					break;
				// deplasare in stanga a dreptunghiului
				case 75:
					move_selector(2);
					redraw_right_area();
					break;
				// deplasare in dreapta a dreptunghiului
				case 77:
					move_selector(3);
					redraw_right_area();
					break;
			}
		}
		else
			switch (ch)
			{
				// zoom in
				case '+':
					move_selector(4);
					redraw_right_area();
					break;
				// zoom out
				case '-':
					move_selector(5);
					redraw_right_area();
					break;
				// tasta "ESC" => terminare program
				case 27:
					gata = 1;
					break;
			}
	}
}

void main()
{
	minf = compute_minf();
	maxf = compute_maxf();
	initialize_graphic_mode();
	draw_borders();
	print_usage_text();
	scale_function();
	initialize_left_area();
	redraw_right_area();
	get_user_input();
	closegraph();
}