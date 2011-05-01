/*
 * Programul permite testarea diferitelor functii 3D ce se pot aplica
 * unei imagini formate din segmente (translatie de-a lungul axelor,
 * scalare, rotatie in jurul axelor, rotatie in jurul unei drepte
 *
 * Autor: Bogdan DUMITRIU
 * Grupa: 3231
 * Data:  09.11.2001
 */

#include <stdio.h>
#include <graphics.h>
#include <conio.h>
#include <stdlib.h>
#include <math.h>

/*
 * Calea catre directorul ce contine Borland Graphical Interface.
 */
#define BGI "C:\\BC31\\BGI"

/*
 * Tipul de proiectie.
 * 	'o' pt. oblica
 *	'p' pt. perspectiva
 */
#define PROIECTIE 'p'

/*
 * Constantele pentru proiectia oblica.
 */
#define R 0.5
#define alpha M_PI/4

/*
 * Constantele pentru proiectia in perspectiva.
 */
#define T 0.5
#define Z0 500

/*
 * Coordonatele initiale ale imaginii.
 */
#define X 0
#define Y 0
#define Z 0

/*
 * Valoarea cu care este translatata imaginea.
 */
#define TRANS 10

/*
 * Factorul cu care este scalata imaginea.
 */
#define SCALE 1.1

/*
 * Unghiul de rotatie.
 */
#define ANGLE M_PI/15

/*
 * Numarul de segmente care formeaza imaginea. Modificarea acestei
 * variabile nu va avea efect direct fara a modifica totodata si
 * alte zone din program.
 */
#define DIM 12

/*
 * Vectori in care se retin coordonatele segmentelor ce formeaza
 * imaginea.
 */
double x1[DIM], y1[DIM], z1[DIM], x2[DIM], y2[DIM], z2[DIM];
double p1[DIM][4], p2[DIM][4];

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
 * Intoarce valoarea rontunjita a parametrului x.
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
 * Coordonata x in proiectia oblica.
 */
int oblic_x(double pct[4])
{
	return round((double) pct[0]+((double) pct[2]*R*cos(alpha)));
}

/*
 * Coordonata y in proiectia oblica.
 */
int oblic_y(double pct[4])
{
	return getmaxy()-round((double) pct[1]+((double) pct[2]*R*sin(alpha)));
}

/*
 * Coordonata x in proiectia perspectiva.
 */
int persp_x(double pct[4])
{
	double aux;

	aux = (double) (pct[2]/(Z0-pct[2]));
	aux = (double) (T*aux);
	aux = (double) (aux+1);
	aux = (double) (pct[0]*aux);

	return round(aux);
}

/*
 * Coordonata y in proiectia perspectiva.
 */
int persp_y(double pct[4])
{
	double aux;

	aux = (double) (pct[2]/(Z0-pct[2]));
	aux = (double) (T*aux);
	aux = (double) (aux+1);
	aux = (double) (pct[1]*aux);

	return getmaxy()-round(aux);
}

/*
 * Calculeaza produsul dintre doua matrice, una de dimensiune 4*4, iar
 * cealalta de dimensiune 4*1.
 */
void product(double big_matrix[4][4], double small_matrix[4],
	double result[4])
{
	double aux[4];

	for (int i = 0; i < 4; i++)
	{
		aux[i] = (double) small_matrix[i];
		result[i] = 0;
	}

	for (i = 0; i < 4; i++)
		for (int j = 0; j < 4; j++)
			result[i] += (double) big_matrix[i][j]*aux[j];
}

/*
 * Initializeaza vectorii ce contin capetele segmentelor ce formeaza
 * imaginea.
 */
void create_figure()
{
	x1[0] = X; y1[0] = Y; z1[0] = Z;
	x2[0] = X+100; y2[0] = Y; z2[0] = Z;

	x1[1] = X+100; y1[1] = Y; z1[1] = Z;
	x2[1] = X+100; y2[1] = Y; z2[1] = Z+100;

	x1[2] = X+100; y1[2] = Y; z1[2] = Z+100;
	x2[2] = X; y2[2] = Y; z2[2] = Z+100;

	x1[3] = X; y1[3] = Y; z1[3] = Z+100;
	x2[3] = X; y2[3] = Y; z2[3] = Z;

	x1[4] = X; y1[4] = Y; z1[4] = Z;
	x2[4] = X; y2[4] = Y+100; z2[4] = Z;

	x1[5] = X; y1[5] = Y+100; z1[5] = Z;
	x2[5] = X+100; y2[5] = Y+100; z2[5] = Z;

	x1[6] = X+100; y1[6] = Y+100; z1[6] = Z;
	x2[6] = X+100; y2[6] = Y; z2[6] = Z;

	x1[7] = X+100; y1[7] = Y+100; z1[7] = Z;
	x2[7] = X+100; y2[7] = Y+100; z2[7] = Z+100;

	x1[8] = X+100; y1[8] = Y+100; z1[8] = Z+100;
	x2[8] = X+100; y2[8] = Y; z2[8] = Z+100;

	x1[9] = X+100; y1[9] = Y+100; z1[9] = Z+100;
	x2[9] = X; y2[9] = Y+100; z2[9] = Z+100;

	x1[10] = X; y1[10] = Y+100; z1[10] = Z+100;
	x2[10] = X; y2[10] = Y; z2[10] = Z+100;

	x1[11] = X; y1[11] = Y+100; z1[11] = Z+100;
	x2[11] = X; y2[11] = Y+100; z2[11] = Z;

	for (int i = 0; i < DIM; i++)
	{
		p1[i][0] = x1[i]; p1[i][1] = y1[i]; p1[i][2] = z1[i]; p1[i][3] = 1;
		p2[i][0] = x2[i]; p2[i][1] = y2[i]; p2[i][2] = z2[i]; p2[i][3] = 1;
	}
}

/*
 * Deseneaza imaginea pe ecran.
 */
void draw_figure()
{
	for (int i = 0; i < DIM; i++)
	{
		if ((i == 2) || (i == 3) || (i == 10))
		{
			setlinestyle(DOTTED_LINE, 1, 1);
			setcolor(WHITE);
		}
		else
		{
			setlinestyle(SOLID_LINE, 1, 1);
			setcolor(RED);
		}

		if (PROIECTIE == 'o')
			line(oblic_x(p1[i]), oblic_y(p1[i]),
				oblic_x(p2[i]), oblic_y(p2[i]));
		if (PROIECTIE == 'p')
			line(persp_x(p1[i]), persp_y(p1[i]),
				persp_x(p2[i]), persp_y(p2[i]));
	}
}

/*
 * Intoarce coordonata x maxima dintre toate capetele segmentelor ce
 * formeaza imaginea.
 */
double maxx()
{
	double max = p1[0][0];
	for (int i = 0; i < DIM; i++)
	{
		if (max < p1[i][0])
			max = p1[i][0];
		if (max < p2[i][0])
			max = p2[i][0];
	}

	return max;
}

/*
 * Intoarce coordonata y maxima dintre toate capetele segmentelor ce
 * formeaza imaginea.
 */
double maxy()
{
	double max = p1[0][1];
	for (int i = 0; i < DIM; i++)
	{
		if (max < p1[i][1])
			max = p1[i][1];
		if (max < p2[i][1])
			max = p2[i][1];
	}

	return max;
}

/*
 * Intoarce coordonata z maxima dintre toate capetele segmentelor ce
 * formeaza imaginea.
 */
double maxz()
{
	double max = p1[0][2];
	for (int i = 0; i < DIM; i++)
	{
		if (max < p1[i][2])
			max = p1[i][2];
		if (max < p2[i][2])
			max = p2[i][2];
	}

	return max;
}

/*
 * Intoarce coordonata x minima dintre toate capetele segmentelor ce
 * formeaza imaginea.
 */
double minx()
{
	double min = p1[0][0];
	for (int i = 0; i < DIM; i++)
	{
		if (min > p1[i][0])
			min = p1[i][0];
		if (min > p2[i][0])
			min = p2[i][0];
	}

	return min;
}

/*
 * Intoarce coordonata y minima dintre toate capetele segmentelor ce
 * formeaza imaginea.
 */
double miny()
{
	double min = p1[0][1];
	for (int i = 0; i < DIM; i++)
	{
		if (min > p1[i][1])
			min = p1[i][1];
		if (min > p2[i][1])
			min = p2[i][1];
	}

	return min;
}

/*
 * Intoarce coordonata z minima dintre toate capetele segmentelor ce
 * formeaza imaginea.
 */
double minz()
{
	double min = p1[0][2];
	for (int i = 0; i < DIM; i++)
	{
		if (min > p1[i][2])
			min = p1[i][2];
		if (min > p2[i][2])
			min = p2[i][2];
	}

	return min;
}

/*
 * Intoarce coordonata x a centrului figurii.
 */
double get_centerx()
{
	return minx()+(maxx()-minx())/2;
}

/*
 * Intoarce coordonata y a centrului figurii.
 */
double get_centery()
{
	return miny()+(maxy()-miny())/2;
}

/*
 * Intoarce coordonata z a centrului figurii.
 */
double get_centerz()
{
	return minz()+(maxz()-minz())/2;
}

/*
 * Translateaza imaginea de-a lungul axei Ox cu pasul TRANS.
 * Daca <dir> este 1 se face deplasare cu +TRANS. Daca <dir>
 * este 0 se face deplasare cu -TRANS.
 */
void translate_Ox(int dir)
{
	double dist = 0;

	if (dir == 1)
		dist = TRANS;
	if (dir == 0)
		dist = -(TRANS);

	double tx[4][4] = {{1,0,0,dist},{0,1,0,0},{0,0,1,0},{0,0,0,1}};

	for (int i = 0; i < DIM; i++)
	{
		product(tx, p1[i], p1[i]);
		product(tx, p2[i], p2[i]);
	}
}

/*
 * Translateaza imaginea de-a lungul axei Oy cu pasul TRANS.
 * Daca <dir> este 1 se face deplasare cu +TRANS. Daca <dir>
 * este 0 se face deplasare cu -TRANS.
 */
void translate_Oy(int dir)
{
	double dist = 0;

	if (dir == 1)
		dist = TRANS;
	if (dir == 0)
		dist = -(TRANS);

	double ty[4][4] = {{1,0,0,0},{0,1,0,dist},{0,0,1,0},{0,0,0,1}};

	for (int i = 0; i < DIM; i++)
	{
		product(ty, p1[i], p1[i]);
		product(ty, p2[i], p2[i]);
	}
}

/*
 * Translateaza imaginea de-a lungul axei Oz cu pasul TRANS.
 * Daca <dir> este 1 se face deplasare cu +TRANS. Daca <dir>
 * este 0 se face deplasare cu -TRANS.
 */
void translate_Oz(int dir)
{
	double dist = 0;

	if (dir == 1)
		dist = TRANS;
	if (dir == 0)
		dist = -(TRANS);

	double tz[4][4] = {{1,0,0,0},{0,1,0,0},{0,0,1,dist},{0,0,0,1}};

	for (int i = 0; i < DIM; i++)
	{
		product(tz, p1[i], p1[i]);
		product(tz, p2[i], p2[i]);
	}
}

/*
 * Translateaza imaginea cu x,y,z.
 */
void translate(double x, double y, double z)
{
	double tz[4][4] = {{1,0,0,x},{0,1,0,y},{0,0,1,z},{0,0,0,1}};

	for (int i = 0; i < DIM; i++)
	{
		product(tz, p1[i], p1[i]);
		product(tz, p2[i], p2[i]);
	}
}

/*
 * Scaleaze imaginea cu factorul SCALE. Daca parametrul <type> este
 * 1 se va face "zoom in". Daca este 0, se va face "zoom out".
 */
void scale(int type)
{
	double factor = 1;
	double cx = get_centerx();
	double cy = get_centery();
	double cz = get_centerz();

	if (type == 1)
		factor = SCALE;
	if (type == 0)
		factor = 1/SCALE;

	double s[4][4] = {{factor,0,0,0},{0,factor,0,0},{0,0,factor,0},{0,0,0,1}};

	for (int i = 0; i < DIM; i++)
	{
		product(s, p1[i], p1[i]);
		product(s, p2[i], p2[i]);
	}

	translate(cx-get_centerx(), cy-get_centery(), cz-get_centerz());
}

/*
 * Rotirea imaginii in jurul dreptei determinate de punctele
 * (x1,y1,z1) si (x2,y2,z2).
 */
void rotate(double x1, double y1, double z1,
	double x2, double y2, double z2)
{
	double A = x2-x1;
	double B = y2-y1;
	double C = z2-z1;
	double V = (double) sqrt((double) (B*B+C*C));
	double L = (double) sqrt((double) (A*A+B*B+C*C));
	double CV = 1;
	if (V != 0)
		CV = (double) (C/V);
	double BV = 0;
	if (V != 0)
		BV = (double) (B/V);
	double VL = 1;
	if (L != 0)
		VL = (double) (V/L);
	double AL = 0;
	if (L != 0)
		AL = (double) (A/L);
	double CA = cos(ANGLE);
	double SA = sin(ANGLE);

	double Rdx[4][4] = {{1,0,0,0},
						{0,CV,-BV,0},
						{0,BV,CV,0},
						{0,0,0,1}};
	double Rix[4][4] = {{1,0,0,0},
						{0,CV,BV,0},
						{0,-BV,CV,0},
						{0,0,0,1}};
	double Rdy[4][4] = {{VL,0,-AL,0},
						{0,1,0,0},
						{AL,0,VL,0},
						{0,0,0,1}};
	double Riy[4][4] = {{VL,0,AL,0},
						{0,1,0,0},
						{-AL,0,VL,0},
						{0,0,0,1}};
	double Rdz[4][4] = {{CA,-SA,0,0},
						{SA,CA,0,0},
						{0,0,1,0},
						{0,0,0,1}};

	translate(-x1, -y1, -z1);
	for (int i = 0; i < DIM; i++)
	{
		product(Rdx, p1[i], p1[i]);
		product(Rdx, p2[i], p2[i]);
		product(Rdy, p1[i], p1[i]);
		product(Rdy, p2[i], p2[i]);
		product(Rdz, p1[i], p1[i]);
		product(Rdz, p2[i], p2[i]);
		product(Riy, p1[i], p1[i]);
		product(Riy, p2[i], p2[i]);
		product(Rix, p1[i], p1[i]);
		product(Rix, p2[i], p2[i]);
	}
	translate(x1, y1, z1);
}

/*
 * Verifica daca cel putin un punct din segmentele care formeaza imaginea
 * este in spatiul vizibil. Daca da, intoarce 0, altfel 1.
 */
int not_on_the_screen()
{
	int res = 1;
	double xx1 = 10, xx2 = 10, yy1 = 10, yy2 = 10;

	for (int i = 0; i < DIM; i++)
	{
		if (PROIECTIE == 'o')
		{
			xx1 = oblic_x(p1[i]);
			xx2 = oblic_x(p2[i]);
			yy1 = oblic_y(p1[i]);
			yy2 = oblic_y(p2[i]);
		}
		if (PROIECTIE == 'p')
		{
			xx1 = persp_x(p1[i]);
			xx2 = persp_x(p2[i]);
			yy1 = persp_y(p1[i]);
			yy2 = persp_y(p2[i]);
		}
		if ((xx1 > 0) && (xx1 < getmaxx()) &&
			(yy1 > 0) && (yy1 < getmaxy()))
			res = 0;
		else
			if ((xx2 > 0) && (xx2 < getmaxx()) &&
				(yy2 > 0) && (yy2 < getmaxy()))
				res = 0;
	}
	return res;
}

void main()
{
	char c;
	int gata = 0;

	initialize_graphic_mode();
	create_figure();

	draw_figure();
	do
	{
		c = getch();
		switch (c)
		{
			case 27:
				gata = 1;
				break;
			case 'a':
				cleardevice();
				translate_Ox(1);
				if (not_on_the_screen())
					translate_Ox(0);
				draw_figure();
				break;
			case 'z':
				cleardevice();
				translate_Ox(0);
				if (not_on_the_screen())
					translate_Ox(1);
				draw_figure();
				break;
			case 's':
				cleardevice();
				translate_Oy(1);
				if (not_on_the_screen())
					translate_Oy(0);
				draw_figure();
				break;
			case 'x':
				cleardevice();
				translate_Oy(0);
				if (not_on_the_screen())
					translate_Oy(1);
				draw_figure();
				break;
			case 'd':
				cleardevice();
				translate_Oz(1);
				if (not_on_the_screen())
					translate_Oz(0);
				draw_figure();
				break;
			case 'c':
				cleardevice();
				translate_Oz(0);
				if (not_on_the_screen())
					translate_Oz(1);
				draw_figure();
				break;
			case '+':
				cleardevice();
				scale(1);
				if (not_on_the_screen())
					scale(0);
				draw_figure();
				break;
			case '-':
				cleardevice();
				scale(0);
				if (not_on_the_screen())
					scale(1);
				draw_figure();
				break;
			case 'q':
				cleardevice();
				rotate(p1[2][0], p1[2][1], p1[2][2], p2[2][0], p2[2][1],
					p2[2][2]);
				draw_figure();
				break;
			case 'w':
				cleardevice();
				rotate(p1[10][0], p1[10][1], p1[10][2], p2[10][0], p2[10][1],
					p2[10][2]);
				draw_figure();
				break;
			case 'e':
				cleardevice();
				rotate(p1[3][0], p1[3][1], p1[3][2], p2[3][0], p2[3][1],
					p2[3][2]);
				draw_figure();
				break;
		}
	}
	while (!gata);

	closegraph();
}