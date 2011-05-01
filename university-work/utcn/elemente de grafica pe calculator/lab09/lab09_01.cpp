/*
 * Algoritm de decupare a unui poligon oarecare folosind
 * un poligon convex ca mijloc de selectare a zonei de decupare.
 *
 * Autor: Bogdan DUMITRIU
 * Data:  14.12.2001
 */

#include <graphics.h>
#include <stdlib.h>
#include <stdio.h>
#include <conio.h>
#include "mouse.h"

#define BGI "C:\\BC31\\BGI"

#define NR_OF_VERTICES 30

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
 * Permite utilizatorului desenarea unui poligon folosind
 * mouse-ul. Parametrul color reprezinta culoarea cu care
 * se va desena poligonul pe ecran. In tabloul points se
 * memora varfurile poligonului. Functia intoarce numarul
 * de varfuri memorate.
 */
int create_polygon(int color, vertex points[NR_OF_VERTICES])
{
	int button, xl, yl;
	int idx = 0;
	int gata = 0;
    POINT pos;

	mouse_on();
	do
	{
		button = mouse_status(&pos);
		if (button == 1)
		{
			points[idx].x = pos.x;
			points[idx++].y = pos.y;
			xl = pos.x;
			yl = pos.y;

			setwritemode(XOR_PUT);
			setcolor(color);
			mouse_restrict(0, 0, getmaxx(), getmaxy());
			mouse_wait_for_release(1, &pos);
			do
			{
				do
				{
					button = mouse_status(&pos);
					if ((pos.x != xl) || (pos.y != yl))
					{
						mouse_off();
						line(points[idx-1].x, points[idx-1].y,
							xl, yl);
						xl = pos.x;
						yl = pos.y;
						line(points[idx-1].x, points[idx-1].y,
							xl, yl);
						mouse_on();
					}
				}
				while ((button != 1) && (button != 2));
				mouse_wait_for_release(button, &pos);
				points[idx].x = xl;
				points[idx++].y = yl;
				gata = 1;
			}
			while (button != 2);
			mouse_wait_for_release(2, &pos);
			mouse_off();
            line(points[idx-1].x, points[idx-1].y, points[0].x, points[0].y);
			//points[idx].x = points[0];
			//points[idx++].y = points[1];
			setwritemode(COPY_PUT);
		}
	}
	while (!gata);
	return idx;
}

/*
 * Deseneaza poligonul dat ca parametru pe ecran.
 */
void draw_polygon(vertex vs[NR_OF_VERTICES], int dim)
{
	setcolor(BLUE);
	for (int i = 0; i < dim-1; i++)
		line(vs[i].x, vs[i].y, vs[i+1].x, vs[i+1].y);
	line(vs[dim-1].x, vs[dim-1].y, vs[0].x, vs[0].y);
}

/*
 * Intoarce constanta m din ecuatia dreptei determinate
 * de punctele (x1,y1), (x2,y2):
 *	y = mx+n
 * Atentie: daca x1 = x2 functia intoarce intotdeauna 0.
 */
double get_m(int x1, int y1, int x2, int y2)
{
	if (x1 != x2)
	{
		double m, n;
		m = (double) y2-y1;
		n = (double) x2-x1;
		m = (double) m/n;
		return m;
	}
	else
		return 0;
}

/*
 * Intoarce constanta n din ecuatia dreptei determinate
 * de punctele (x1,y1), (x2,y2):
 *	y = mx+n
 * Atentie: daca x1 = x2 functia intoarce intotdeauna 0.
 */
double get_n(int x1, int y1, int x2, int y2)
{
	if (x1 != x2)
	{
		double m, n;
		m = (double) y2-y1;
		n = (double) x2-x1;
		m = (double) m/n;
		n = (double) x1*m;
		n = (double) y1-n;
		return n;
	}
	else
		return 0;
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
		v.x = x1;
		double aux = (double) (v.x-p.x)*(s.y-p.y);
		aux = (double) aux/(s.x-p.x);
		aux = (double) aux+p.y;
		v.y = aux;
	}
	else
		if (y1 == y2)
		{
			v.y = y1;
			double aux = (double) (v.y-p.y)*(s.x-p.x);
			aux = (double) aux/(s.y-p.y);
			aux = (double) aux+p.x;
			v.x = aux;
		}
		else
		{
			double m1 = get_m(x1, y1, x2, y2);
			double n1 = get_n(x1, y1, x2, y2);
			if (s.x != p.x)
			{
				double m2 =	get_m(s.x, s.y, p.x, p.y);
				double n2 =	get_n(s.x, s.y, p.x, p.y);

				double aux1 = (double) m1-m2;
				double aux2 = (double) n2-n1;
				double aux = (double) aux2/aux1;
				v.x = aux;
				aux = (double) m1*aux;
				aux = (double) n1+aux;
				v.y = aux;
			}
			else
			{
				v.x = s.x;
				double aux = (double) m1*v.x;
				aux = (double) n1+aux;
				v.y = aux;
			}
		}

	return v;
}

/*
 * Intoarce 1 daca varful v este de aceeasi partea a dreptei
 * definita de punctele (x1,y1) si (x2,y2) ca si punctul (x,y)
 * si 0 in caz contrar.
 */
int position(vertex v, int x1, int y1, int x2, int y2, int x, int y)
{
	if (x1 != x2)
	{
		double m, n;
		double aux1, aux2;

		m = get_m(x1, y1, x2, y2);
		n =	get_n(x1, y1, x2, y2);

		aux1 = (double) m*v.x;
		aux1 = (double) aux1+n;
		aux1 = (double) v.y-aux1;
		aux2 = (double) m*x;
		aux2 = (double) aux2+n;
		aux2 = (double) y-aux2;

		if (((double) aux1*aux2) > 0)
			return 1;
		else
			return 0;
	}
	else
	{
		if (((v.x < x1) && (x < x1)) || ((v.x > x1) && (x > x1)))
			return 1;
		else
			return 0;
	}
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
 * Un varf este in interior daca este de aceeasi parte a
 * dreptei ca si punctul (x,y).
 */
int side_clipping(vertex vin[2*NR_OF_VERTICES], int dim_vin,
	vertex vout[2*NR_OF_VERTICES], int x1, int y1, int x2, int y2,
	int x, int y)
{
	vertex i, p, s;
	int idx = 0;

	s = vin[dim_vin-1];
	for (int j = 0; j < dim_vin; j++)
	{
		p = vin[j];
		if (position(p, x1, y1, x2, y2, x, y))		// p in interior
			if (position(s, x1, y1, x2, y2, x, y))	// s in interior
				vout[idx++] = p;
			else									// s in exterior
			{
				vout[idx++] = intersection(s, p, x1, y1, x2, y2);
				vout[idx++] = p;
			}
		else										// p in exterior
			if (position(s, x1, y1, x2, y2, x, y))	// s in interior
				vout[idx++] = intersection(s, p, x1, y1, x2, y2);
		s = p;
	}
	return idx;
}

/*
 * Intorce in vout poligonul din vin modificat prin taierea
 * partilor care nu se afla in poligonul sel_poly cu dim_sel
 * varfuri. Functia intoarce dimensiunea lui vout.
 */
int polygon_clipping(vertex vin[NR_OF_VERTICES], int dim,
	vertex vout[2*NR_OF_VERTICES], vertex sel_poly[NR_OF_VERTICES],
	int dim_sel)
{
	for (int i = 0; i < dim_sel-2; i++)
	{
		dim = side_clipping(vin, dim, vout, sel_poly[i].x, sel_poly[i].y,
			sel_poly[i+1].x, sel_poly[i+1].y, sel_poly[i+2].x,
			sel_poly[i+2].y);
		for (int j = 0; j < dim; j++)
			vin[j] = vout[j];
	}
	dim = side_clipping(vin, dim, vout, sel_poly[dim_sel-2].x,
		sel_poly[dim_sel-2].y, sel_poly[dim_sel-1].x, sel_poly[dim_sel-1].y,
		sel_poly[0].x, sel_poly[0].y);
	for (int j = 0; j < dim; j++)
		vin[j] = vout[j];
	dim = side_clipping(vin, dim, vout, sel_poly[dim_sel-1].x,
		sel_poly[dim_sel-1].y, sel_poly[0].x, sel_poly[0].y, sel_poly[1].x,
		sel_poly[1].y);
	for (j = 0; j < dim; j++)
		vin[j] = vout[j];

	return dim;
}

void main()
{
	vertex vs[3*NR_OF_VERTICES];
	vertex vf[3*NR_OF_VERTICES];
	vertex sel_poly[NR_OF_VERTICES];
	int sir[6*NR_OF_VERTICES];
	int dim, dim_sel;

	initialize_graphic_mode();

	dim = create_polygon(RED, vs);
	dim_sel = create_polygon(LIGHTGREEN, sel_poly);

	dim = polygon_clipping(vs, dim, vf, sel_poly, dim_sel);

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