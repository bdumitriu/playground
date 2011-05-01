/*
 * Algoritmul lui Sutherland-Hodgman de decupare a poligoanelor.
 *
 * Autor: Bogdan DUMITRIU
 * Data:  08.12.2001
 */

#include <graphics.h>
#include <stdlib.h>
#include <stdio.h>
#include <conio.h>

#define BGI "C:\\BC31\\BGI"

#define NR_OF_VERTICES 5

struct vertex
{
    int x;
	int y;
};

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
 * Genereaza un poligon aleator.
 */
void generate_polygon(vertex vs[NR_OF_VERTICES])
{
	randomize();
	for (int i = 0; i < NR_OF_VERTICES; i++)
	{
		vs[i].x = random(getmaxx());
		vs[i].y = random(getmaxy());
	}
}

/*
 * Deseneaza poligonul dat ca parametru pe ecran.
 */
void draw_polygon(vertex vs[NR_OF_VERTICES])
{
	setcolor(RED);
	for (int i = 0; i < NR_OF_VERTICES-1; i++)
		line(vs[i].x, vs[i].y, vs[i+1].x, vs[i+1].y);
	line(vs[NR_OF_VERTICES-1].x, vs[NR_OF_VERTICES-1].y, vs[0].x, vs[0].y);
}

/*
 * Intoarce varful ce reprezinta intersectia dintre segmentul
 * determinat de varfurile s si p si segmentul determinat de
 * punctele (x1,y1) si (x2,y2).
 */
vertex intersection(vertex s, vertex p, int x1, int y1, int x2, int y2)
{
	vertex v;
	if (x1 == x2)
	{
		if (y1 > y2)
			v.x = x1-1;
		else
			v.x = x1+1;
		double aux = (double) (v.x-p.x)*(s.y-p.y);
		aux = (double) aux/(s.x-p.x);
		aux = (double) aux+p.y;
		v.y = aux;
	}
	else
	{
		if (x1 > x2)
			v.y = y1+1;
		else
			v.y = y1-1;
		double aux = (double) (v.y-p.y)*(s.x-p.x);
		aux = (double) aux/(s.y-p.y);
		aux = (double) aux+p.x;
		v.x = aux;
	}

	return v;
}

/*
 * Intoarce 1 daca varful v este considerat a fi de partea
 * interioara a dreptei definita de punctele (x1,y1) si (x2,y2)
 * si 0 in caz contrar.
 */
int position(vertex v, int x1, int y1, int x2, int y2)
{
	if (x1 == x2)
	{
		if (y1 < y2)
		{
			if (v.x > x1)
				return 1;
		}
		else
		{
			if (v.x < x2)
				return 1;
		}
	}
	else
	{
		if (x1 < x2)
		{
			if (v.y < y2)
				return 1;
		}
		else
		{
			if (v.y > y1)
				return 1;
		}
	}
	return 0;
}

/*
 * Creeaza din lista varfurilor de intrare (primele dim_vin
 * elemente din vin) o lista a varfurilor de iesire (vout)
 * taind acele varfuri care sunt in exteriorul dreptei
 * definita de punctele (x1,y1) si (x2,y2) si adaugand
 * punctele de intersectie cu dreapta. Functia intoarce
 * dimensiunea vectorului vout. Lista varfurilor de intrare
 * nu trebuie sa aiba vin[0] = vin[dim_vin-1].
 *
 * Definirea notiunilor de interior si exterior a dreptei se
 * face prin functia position definita mai sus.
 */
int side_clipping(vertex vin[2*NR_OF_VERTICES], int dim_vin,
	vertex vout[2*NR_OF_VERTICES], int x1, int y1, int x2, int y2)
{
	vertex i, p, s;
	int idx = 0;

	s = vin[dim_vin-1];
	for (int j = 0; j < dim_vin; j++)
	{
		p = vin[j];
		if (position(p, x1, y1, x2, y2))		// p in interior
			if (position(s, x1, y1, x2, y2))    // s in interior
				vout[idx++] = p;
			else								// s in exterior
			{
				vout[idx++] = intersection(s, p, x1, y1, x2, y2);
				vout[idx++] = p;
			}
		else									// p in exterior
			if (position(s, x1, y1, x2, y2))	// s in interior
				vout[idx++] = intersection(s, p, x1, y1, x2, y2);
		s = p;
	}
	return idx;
}

/*
 * Intorce in vout poligonul din vin modificat prin taierea
 * partilor care nu se afla in dreptunghiul cu coltul din
 * stanga sus in (x1,y1) si cel din dreapta jos in (x2,y2).
 * Functia intoarce dimensiunea lui vout.
 */
int polygon_clipping(vertex vin[NR_OF_VERTICES],
	vertex vout[2*NR_OF_VERTICES], int x1, int y1, int x2, int y2)
{
	int dim;

	dim = side_clipping(vin, NR_OF_VERTICES, vout, x1, y2, x2, y2);
	for (int i = 0; i < dim; i++)
		vin[i] = vout[i];
	dim = side_clipping(vin, dim, vout, x2, y2, x2, y1);
	for (i = 0; i < dim; i++)
		vin[i] = vout[i];
	dim = side_clipping(vin, dim, vout, x2, y1, x1, y1);
	for (i = 0; i < dim; i++)
		vin[i] = vout[i];
	dim = side_clipping(vin, dim, vout, x1, y1, x1, y2);

	return dim;
}

void main()
{
	vertex vs[5*NR_OF_VERTICES];
	vertex vf[5*NR_OF_VERTICES];
	int sir[5*NR_OF_VERTICES];
	int dim;

	initialize_graphic_mode();
	generate_polygon(vs);
	draw_polygon(vs);
	setcolor(MAGENTA);
	rectangle(100, 100, 400, 400);
	getch();
	dim = polygon_clipping(vs, vf, 100, 100, 400, 400);

	for (int i = 0; i < dim; i++)
	{
		sir[i*2] = vf[i].x;
		sir[i*2+1] = vf[i].y;
	}
	sir[2*dim] = vf[0].x;
	sir[2*dim+1] = vf[0].y;

	setfillstyle(SOLID_FILL, YELLOW);
	setcolor(RED);
	fillpoly(dim, sir);

	getch();
	closegraph();
}