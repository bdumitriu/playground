/*
 * Programul permite testarea diferitelor functii 2D ce se pot aplica
 * unei imagini formate din segmente (translatie, scalare, rotatie
 * in jurul originii sau a centrului imaginii, simetrizare fata de
 * origine, axa Ox, axa Oy sau o dreapta oarecare).
 *
 * Autor: Bogdan DUMITRIU
 * Grupa: 3231
 * Data:  02.11.2001
 */

#include <stdio.h>
#include <graphics.h>
#include <conio.h>
#include <stdlib.h>
#include <math.h>

/*
 * Numarul de segmente care formeaza imaginea. Modificarea acestei
 * variabile nu va avea efect direct fara a modifica totodata si
 * alte zone din program.
 */
#define DIM 4

/*
 * Calea catre directorul ce contine Borland Graphical Interface.
 */
#define BGI "C:\\BC31\\BGI"

/*
 * Vectori in care se retin coordonatele segmentelor ce formeaza
 * imaginea.
 */
double x1[DIM], y1[DIM], x2[DIM], y2[DIM];

/*
 * Punctul de intersectie al axelor.
 */
int x = 298, y = 302;

/*
 * Distantele de translatie.
 */
int transX = 0, transY = 0;

/*
 * Factorii de scalare.
 */
double scaleX = 1, scaleY = 1;

/*
 * Unghiul de rotatie.
 */
double rotA = 0;

/*
 * Coordonatele dreptei oarecare.
 */
int simX = 0, simY = 0;
double simA = 0;

int idx = 1;

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
 * Initializeaza vectorii ce contin capetele segmentelor ce formeaza
 * imaginea.
 */
void create_figure()
{
	x1[0] = 100;
	y1[0] = 200;
	x2[0] = 200;
	y2[0] = 200;

	x1[1] = 200;
	y1[1] = 200;
	x2[1] = 200;
	y2[1] = 100;

	x1[2] = 200;
	y1[2] = 100;
	x2[2] = 100;
	y2[2] = 100;

	x1[3] = 100;
	y1[3] = 100;
	x2[3] = 100;
	y2[3] = 200;
}

/*
 * Intoarce coordonata x maxima dintre toate capetele segmentelor ce
 * formeaza imaginea.
 */
double maxx()
{
	double max = x1[0];
	for (int i = 0; i < DIM; i++)
	{
		if (max < x1[i])
			max = x1[i];
		if (max < x2[i])
			max = x2[i];
	}

	return max;
}

/*
 * Intoarce coordonata y maxima dintre toate capetele segmentelor ce
 * formeaza imaginea.
 */
double maxy()
{
	double max = y1[0];
	for (int i = 0; i < DIM; i++)
	{
		if (max < y1[i])
			max = y1[i];
		if (max < y2[i])
			max = y2[i];
	}

	return max;
}

/*
 * Intoarce coordonata x minima dintre toate capetele segmentelor ce
 * formeaza imaginea.
 */
double minx()
{
	double min = x1[0];
	for (int i = 0; i < DIM; i++)
	{
		if (min > x1[i])
			min = x1[i];
		if (min > x2[i])
			min = x2[i];
	}

	return min;
}

/*
 * Intoarce coordonata y minima dintre toate capetele segmentelor ce
 * formeaza imaginea.
 */
double miny()
{
	double min = y1[0];
	for (int i = 0; i < DIM; i++)
	{
		if (min > y1[i])
			min = y1[i];
		if (min > y2[i])
			min = y2[i];
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
 * Intoarce coordonata x a centrului figurii.
 */
double get_centery()
{
	return miny()+(maxy()-miny())/2;
}

/*
 * Seteaza viewport-ul curent in functie de parametrul v la niste valori
 * predefinite.
 */
void set(int v)
{
	setcolor(MAGENTA);
	switch (v)
	{
		case 0:
			setviewport(0, 50, getmaxx(), getmaxy(), 1);
			setwritemode(COPY_PUT);
			rectangle(0, 0, getmaxx(), getmaxy()-50);
			setviewport(1, 51, getmaxx()-1, getmaxy()-1, 1);
			break;
		case 1:
			setviewport(10, 5, 100, 45, 1);
			break;
		case 2:
			setviewport(110, 5, 200, 45, 1);
			break;
		case 3:
			setviewport(210, 5, 300, 45, 1);
			break;
		case 4:
			setviewport(310, 5, 500, 45, 1);
			break;
		case 5:
			setviewport(490, 0, getmaxx(), 48, 1);
			break;
	}
	setcolor(CYAN);
}

/*
 * Deseneaza toate segmentele.
 */
void draw_figure()
{
	set(0);
	setwritemode(XOR_PUT);
	setcolor(CYAN);
	for (int i = 0; i < DIM; i++)
	{
		line(round(x1[i]), round(y1[i]), round(x2[i]), round(y2[i]));
	}
}

/*
 * Deseneaza axele de coordonate.
 */
void draw_axes()
{
	set(0);
	setwritemode(XOR_PUT);
	setcolor(WHITE);
	line(x, 0, x, getmaxy());
	line(0, y, getmaxx(), y);
}

/*
 * Deseneaza linia aleatoare
 */
void draw_line()
{
	set(0);
	setwritemode(XOR_PUT);
	setcolor(BLUE);
	if (simA < 0.09)
		line(0, simY, 638, simY);
	else
		if ((simA > 4.65) && (simA < 4.75))
			line(simX, 0, simX, 428);
		else
			line(0, round(simY+((double) simX*tan(simA))),
				638, round(simY-((double) (638-simX)*tan(simA))));
}

/*
 * Translateaza imaginea cu tx pe axa Ox si cu ty pe axa Oy.
 */
void translate(double tx, double ty)
{
	for (int i = 0; i < DIM; i++)
	{
		x1[i] = (double) tx+x1[i];
		y1[i] = (double) ty+y1[i];
		x2[i] = (double) tx+x2[i];
		y2[i] = (double) ty+y2[i];
	}
}

/*
 * Scaleaza imaginea cu factorul sx pe axa Ox si cu factorul sy pe
 * axa Oy.
 */
void scale(double sx, double sy)
{
	double cx = get_centerx();
	double cy = get_centery();

	for (int i = 0; i < DIM; i++)
	{
		x1[i] = (double) sx*x1[i];
		y1[i] = (double) sy*y1[i];
		x2[i] = (double) sx*x2[i];
		y2[i] = (double) sy*y2[i];
	}

	translate(cx-get_centerx(), cy-get_centery());
}

/*
 * Roteste imaginea cu unghiul alpha in jurul punctului (cx,cy).
 */
void rotate(double alpha, double cx, double cy)
{
	translate(-cx, -cy);
	for (int i = 0; i < DIM; i++)
	{
		double xx1 = x1[i], xx2 = x2[i], yy1 = -y1[i], yy2 = -y2[i];
		x1[i] = xx1*cos(alpha)-yy1*sin(alpha);
		y1[i] = -(xx1*sin(alpha)+yy1*cos(alpha));
		x2[i] = xx2*cos(alpha)-yy2*sin(alpha);
		y2[i] = -(xx2*sin(alpha)+yy2*cos(alpha));
	}
	translate(cx, cy);
}

/*
 * Roteste imaginea cu unghiul alpha in jurul punctului originii.
 */
void origin_rotate(double alpha)
{
	rotate(alpha, x, y);
}

/*
 * Roteste imaginea cu unghiul alpha in jurul centrului imaginii.
 */
void center_rotate(double alpha)
{
	rotate(alpha, get_centerx(), get_centery());
}

/*
 * Muta imaginea simetric fata de punctul (getmaxx()/2,getmaxy()/2).
 */
void origin_simetry()
{
	translate(-x, -y);
	for (int i = 0; i < DIM; i++)
	{
		x1[i] = -x1[i];
		y1[i] = -y1[i];
		x2[i] = -x2[i];
		y2[i] = -y2[i];
	}
	translate(x, y);
}

/*
 * Muta imaginea simetric fata de dreapta paralela cu axa Ox ce
 * trece prin getmaxx()/2.
 */
void Ox_simetry()
{
	translate(0, -y);
	for (int i = 0; i < DIM; i++)
	{
		y1[i] = -y1[i];
		y2[i] = -y2[i];
	}
	translate(0, y);
}

/*
 * Muta imaginea simetric fata de dreapta paralela cu axa Oy ce
 * trece prin getmaxy()/2.
 */
void Oy_simetry()
{
	translate(-x, 0);
	for (int i = 0; i < DIM; i++)
	{
		x1[i] = -x1[i];
		x2[i] = -x2[i];
	}
	translate(x, 0);
}

/*
 * Muta imaginea fata de dreapta ce trece prin (dx,dy) si are panta
 * tg(alpha).
 */
void random_line_simetry(int dx, int dy, double alpha)
{
	int oldy = y;

	translate(-dx, -dy);
	rotate(-alpha, 0, 0);
	y = 0;
	Ox_simetry();
	y = oldy;
	rotate(alpha, 0, 0);
	translate(dx, dy);
}

/*
 * Deseneaza dreptunghiurile in care se introduc valorile.
 */
void variable_space(int direction)
{
	setcolor(RED);
	setwritemode(COPY_PUT);
	switch (idx)
	{
		case 1:
			setcolor(RED);
			setwritemode(COPY_PUT);
			if (direction == 1)
			{
				set(4);
				setcolor(RED);
				setwritemode(COPY_PUT);
				rectangle(90+textwidth("р = "), 13,
					135+textwidth("р = "), 15+textheight("1")+2);
			}
			else
			{
				set(1);
				setcolor(RED);
				setwritemode(COPY_PUT);
				rectangle(10+textwidth("y = "), 28,
					45+textwidth("y = "), 30+textheight("1")+2);
			}
			set(1);
			setcolor(WHITE);
			rectangle(10+textwidth("x = "), 13,
				45+textwidth("x = "), 15+textheight("1")+2);
			break;
		case 2:
			if (direction == 1)
			{
				set(1);
				setcolor(RED);
				setwritemode(COPY_PUT);
				rectangle(10+textwidth("x = "), 13,
					45+textwidth("x = "), 15+textheight("1")+2);
			}
			else
			{
				set(2);
				setcolor(RED);
				setwritemode(COPY_PUT);
				rectangle(10+textwidth("x = "), 13,
					55+textwidth("x = "), 15+textheight("1")+2);
			}
			set(1);
			setcolor(WHITE);
			rectangle(10+textwidth("y = "), 28,
				45+textwidth("y = "), 30+textheight("1")+2);
			break;
		case 3:
			if (direction == 1)
			{
				set(1);
				setcolor(RED);
				setwritemode(COPY_PUT);
				rectangle(10+textwidth("y = "), 28,
					45+textwidth("y = "), 30+textheight("1")+2);
			}
			else
			{
				set(2);
				setcolor(RED);
				setwritemode(COPY_PUT);
				rectangle(10+textwidth("y = "), 28,
					55+textwidth("y = "), 30+textheight("1")+2);
			}
			set(2);
			setcolor(WHITE);
			rectangle(10+textwidth("x = "), 13,
				55+textwidth("x = "), 15+textheight("1")+2);
			break;
		case 4:
			if (direction == 1)
			{
				set(2);
				setcolor(RED);
				setwritemode(COPY_PUT);
				rectangle(10+textwidth("x = "), 13,
					55+textwidth("x = "), 15+textheight("1")+2);
			}
			else
			{
				set(3);
				setcolor(RED);
				setwritemode(COPY_PUT);
				rectangle(10+textwidth("р = "), 13,
					55+textwidth("р = "), 15+textheight("1")+2);
			}
			set(2);
			setcolor(WHITE);
			rectangle(10+textwidth("y = "), 28,
				55+textwidth("y = "), 30+textheight("1")+2);
			break;
		case 5:
			if (direction == 1)
			{
				set(2);
				setcolor(RED);
				setwritemode(COPY_PUT);
				rectangle(10+textwidth("y = "), 28,
					55+textwidth("y = "), 30+textheight("1")+2);
			}
			else
			{
				set(4);
				setcolor(RED);
				setwritemode(COPY_PUT);
				rectangle(10+textwidth("x = "), 13,
					45+textwidth("x = "), 15+textheight("1")+2);
			}
			set(3);
			setcolor(WHITE);
			rectangle(10+textwidth("р = "), 13,
				55+textwidth("р = "), 15+textheight("1")+2);
			break;
		case 6:
			if (direction == 1)
			{
				set(3);
				setcolor(RED);
				setwritemode(COPY_PUT);
				rectangle(10+textwidth("р = "), 13,
					55+textwidth("р = "), 15+textheight("1")+2);
			}
			else
			{
				set(4);
				setcolor(RED);
				setwritemode(COPY_PUT);
				rectangle(10+textwidth("y = "), 28,
					45+textwidth("y = "), 30+textheight("1")+2);
			}
			set(4);
			setcolor(WHITE);
			rectangle(10+textwidth("x = "), 13,
				45+textwidth("x = "), 15+textheight("1")+2);
			break;
		case 7:
			if (direction == 1)
			{
				set(4);
				setcolor(RED);
				setwritemode(COPY_PUT);
				rectangle(10+textwidth("x = "), 13,
					45+textwidth("x = "), 15+textheight("1")+2);
			}
			else
			{
				set(4);
				setcolor(RED);
				setwritemode(COPY_PUT);
				rectangle(90+textwidth("р = "), 13,
					135+textwidth("р = "), 15+textheight("1")+2);
			}
			set(4);
			setcolor(WHITE);
			rectangle(10+textwidth("y = "), 28,
				45+textwidth("y = "), 30+textheight("1")+2);
			break;
		case 8:
			if (direction == 1)
			{
				set(4);
				setcolor(RED);
				setwritemode(COPY_PUT);
				rectangle(10+textwidth("y = "), 28,
					45+textwidth("y = "), 30+textheight("1")+2);
			}
			else
			{
				set(1);
				setcolor(RED);
				setwritemode(COPY_PUT);
				rectangle(10+textwidth("x = "), 13,
					45+textwidth("x = "), 15+textheight("1")+2);
			}
			set(4);
			setcolor(WHITE);
			rectangle(90+textwidth("р = "), 13,
				135+textwidth("р = "), 15+textheight("1")+2);
			break;
	}
}

/*
 * Transforma un double in string.
 */
void dtoa(double d, char s[5])
{
	char *str = (char*) malloc(5*sizeof(char));
	int aux1, aux2;
	int aux = (int) d;

	if ((d - aux) > 0.96)
		aux++;

	itoa(aux, str, 10);
	s[0] = str[0];
	s[1] = '.';
	d = d - ((int) d);
	if ((d >= 0.96) || (d <= 0.04))
	{
		s[2] = '0';
		s[3] = '0';
		s[4] = '\0';
	}
	else
		if ((d > 0.04) && (d <= 0.09))
		{
			s[2] = '0';
			s[3] = '5';
			s[4] = '\0';
		}
		else
		{
			str = (char*) ecvt(d, 2, &aux1, &aux2);
			s[2] = str[0];
			s[3] = str[1];
			s[4] = '\0';
		}
	free(s);
}

/*
 * Tipareste valorile in dreptunghiuri.
 */
void print_numbers()
{
	char s[5];

	switch (idx)
	{
		case 1:
			set(1);
			itoa(transX, s, 10);
			setcolor(BLACK);
			outtextxy(13+textwidth("x = "), 16, "лллл");
			setcolor(RED);
			outtextxy(15+textwidth("x = "), 16, s);
			break;
		case 2:
			set(1);
			itoa(transY, s, 10);
			setcolor(BLACK);
			outtextxy(13+textwidth("y = "), 31, "лллл");
			setcolor(RED);
			outtextxy(15+textwidth("y = "), 31, s);
			break;
		case 3:
			set(2);
			dtoa(scaleX, s);
			setcolor(BLACK);
			outtextxy(13+textwidth("x = "), 16, "ллллл");
			setcolor(RED);
			outtextxy(15+textwidth("x = "), 16, s);
			break;
		case 4:
			set(2);
			dtoa(scaleY, s);
			setcolor(BLACK);
			outtextxy(13+textwidth("y = "), 31, "ллллл");
			setcolor(RED);
			outtextxy(15+textwidth("y = "), 31, s);
			break;
		case 5:
			set(3);
			dtoa(rotA, s);
			setcolor(BLACK);
			outtextxy(13+textwidth("р = "), 16, "ллллл");
			setcolor(RED);
			outtextxy(15+textwidth("р = "), 16, s);
			break;
		case 6:
			set(4);
			itoa(simX, s, 10);
			setcolor(BLACK);
			outtextxy(13+textwidth("x = "), 16, "лллл");
			setcolor(RED);
			outtextxy(15+textwidth("x = "), 16, s);
			break;
		case 7:
			set(4);
			itoa(simY, s, 10);
			setcolor(BLACK);
			outtextxy(13+textwidth("y = "), 31, "лллл");
			setcolor(RED);
			outtextxy(15+textwidth("y = "), 31, s);
			break;
		case 8:
			set(4);
			dtoa(simA, s);
			setcolor(BLACK);
			outtextxy(93+textwidth("р = "), 16, "ллллл");
			setcolor(RED);
			outtextxy(95+textwidth("р = "), 16, s);
			break;
	}
}

/*
 * Initializeaza toate spatiile de editare.
 */
void initialize_variable_spaces()
{
	set(1);
	setcolor(RED);
	outtextxy(0, 0, "Transl. cu:");
	idx = 1;
	outtextxy(10, 15, "x = ");
	variable_space(1);
	print_numbers();
	idx = 2;
	setcolor(RED);
	outtextxy(10, 30, "y = ");
	variable_space(1);
	print_numbers();

	set(2);
	setcolor(RED);
	outtextxy(0, 0, "Scalare cu:");
	idx = 3;
	outtextxy(10, 15, "x = ");
	variable_space(1);
	print_numbers();
	idx = 4;
	setcolor(RED);
	outtextxy(10, 30, "y = ");
	variable_space(1);
	print_numbers();

	set(3);
	setcolor(RED);
	outtextxy(0, 0, "Rotire cu:");
	idx = 5;
	outtextxy(10, 15, "р = ");
	variable_space(1);
	print_numbers();

	set(4);
	setcolor(RED);
	outtextxy(0, 0, "Coord. dr. aleatoare:");
	idx = 6;
	outtextxy(10, 15, "x = ");
	variable_space(1);
	print_numbers();
	idx = 7;
	setcolor(RED);
	outtextxy(10, 30, "y = ");
	variable_space(1);
	print_numbers();
	idx = 8;
	setcolor(RED);
	outtextxy(90, 15, "р = ");
	variable_space(1);
	print_numbers();

	idx = 1;
	variable_space(1);

	set(5);
	setcolor(RED);
	outtextxy(1, 5, "Comenzi:");
	outtextxy(1, 15, "+, -, t, s, r, c");
	outtextxy(1, 25, "x, y, o, d");
	outtextxy(1, 37, "ESC, sageti, TAB");

	setviewport(0, 0, getmaxx(), getmaxy(), 1);
	setcolor(RED);
	line(5, 3, 5, 47);
	line(105, 3, 105, 47);
	line(205, 3, 205, 47);
	line(305, 3, 305, 47);
	line(485, 3, 485, 47);
	line(635, 3, 635, 47);
	line(5, 2, 635, 2);
	line(5, 48, 635, 48);
}

/*
 * Modifica valoarea valorilor in urma comenzii date de utilizator.
 */
void modify_variable(int way)
{
	switch (idx)
	{
		case 1:
			if ((transX < 200) && (way == 1))
				transX++;
			if ((transX > -200) && (way == 0))
				transX--;
			break;
		case 2:
			if ((transY < 200) && (way == 1))
				transY++;
			if ((transY > -200) && (way == 0))
				transY--;
			break;
		case 3:
			if ((scaleX < 4.96) && (way == 1))
				scaleX += 0.05;
			if ((scaleX > 0.05) && (way == 0))
				scaleX -= 0.05;
			break;
		case 4:
			if ((scaleY < 4.96) && (way == 1))
				scaleY += 0.05;
			if ((scaleY > 0.05) && (way == 0))
				scaleY -= 0.05;
			break;
		case 5:
			if ((rotA < 2*M_PI) && (way == 1))
				rotA += 0.1;
			if ((rotA > 0) && (way == 0))
				rotA -= 0.1;
			break;
		case 6:
			draw_line();
			if ((simX < getmaxx()-7) && (way == 1))
				simX += 5;
			if ((simX > 0) && (way == 0))
				simX -= 5;
			draw_line();
			break;
		case 7:
			draw_line();
			if ((simY < getmaxy()-57) && (way == 1))
				simY += 5;
			if ((simY > 0) && (way == 0))
				simY -= 5;
			draw_line();
			break;
		case 8:
			draw_line();
			if ((simA < 3.09) && (way == 1))
				simA += 0.1;
			if ((simA > 0) && (way == 0))
				simA -= 0.1;
			draw_line();
			break;
	}
	print_numbers();
}

/*
 * Verifica daca cel putin un punct din segmentele care formeaza imaginea
 * este in spatiul vizibil. Daca da, intoarce 0, altfel 1.
 */
int not_on_the_screen()
{
	int res = 1;

	for (int i = 0; i < DIM; i++)
	{
		if (((x1[i] >= 0) && (x1[i] <= 638) &&
			(y1[i] >= 0) && (y1[i] <= 428)) ||
			((x2[i] >= 0) && (x2[i] <= 638) &&
			(y2[i] >= 0) && (y2[i] <= 428)))
			res = 0;
	}
	return res;
}

void main()
{
	char c;
	int gata = 0;
	double xx1[DIM], xx2[DIM], yy1[DIM], yy2[DIM];

	initialize_graphic_mode();
	initialize_variable_spaces();
	create_figure();
	draw_figure();
	draw_axes();
	draw_line();

	do
	{
		c = getch();
		for (int i = 0; i < DIM; i++)
		{
			xx1[i] = x1[i];
			xx2[i] = x2[i];
			yy1[i] = y1[i];
			yy2[i] = y2[i];
		}
		switch (c)
		{
			case 9:
				idx++;
				if (idx == 9)
					idx = 1;
				variable_space(1);
				break;
			case '+':
				modify_variable(1);
				break;
			case '-':
				modify_variable(0);
				break;
			case 't':
				draw_figure();
				translate(transX, transY);
				draw_figure();
				break;
			case 's':
				draw_figure();
				scale(scaleX, scaleY);
				draw_figure();
				break;
			case 'c':
				draw_figure();
				center_rotate(rotA);
				draw_figure();
				break;
			case 'r':
				draw_figure();
				origin_rotate(rotA);
				draw_figure();
				break;
			case 'x':
				draw_figure();
				Ox_simetry();
				draw_figure();
				break;
			case 'y':
				draw_figure();
				Oy_simetry();
				draw_figure();
				break;
			case 'o':
				draw_figure();
				origin_simetry();
				draw_figure();
				break;
			case 'd':
				draw_figure();
				random_line_simetry(simX, simY, simA);
				draw_figure();
				break;
			case 27:
				gata = 1;
				break;
			case 0:
				c = getch();
				switch (c)
				{
					case 15:
						idx--;
						if (idx == 0)
							idx = 8;
						variable_space(0);
						break;
					case 80:
						draw_axes();
						if (y < 427)
							y++;
						draw_axes();
						break;
					case 72:
						draw_axes();
						if (y > 0)
							y--;
						draw_axes();
						break;
					case 77:
						draw_axes();
						if (x < 638)
							x++;
						draw_axes();
						break;
					case 75:
						draw_axes();
						if (x > 0)
							x--;
						draw_axes();
						break;
				}
		}
		if (not_on_the_screen())
		{
			draw_figure();
			for (int i = 0; i < DIM; i++)
			{
				x1[i] = xx1[i];
				x2[i] = xx2[i];
				y1[i] = yy1[i];
				y2[i] = yy2[i];
			}
			draw_figure();
		}
	}
	while (!gata);

	closegraph();
}