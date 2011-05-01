#include <stdlib.h>
#include <conio.h>
#include <stdio.h>
#include <math.h>
#include <values.h>
#include <graphics.h>
#include <string.h>

/*
 * Calea catre directorul ce contine Borland Graphical Interface.
 */
#define BGI "BGI"

int m = -1, n;
int x[100];
int y[100][10];
int color[10];
char text[12][100];
int ming, maxg;
int a, b;

int compute_max()
{
	int max = -MAXINT;
	for (int i = 0; i < m; i++)
		for (int j = 0; j < n; j++)
			if (max < y[i][j])
				max = y[i][j];

	return max;
}

int compute_min()
{
	int min = MAXINT;
	for (int i = 0; i < m; i++)
		for (int j = 0; j < n; j++)
			if (min > y[i][j])
				min = y[i][j];

	return min;
}

void initialize_a_b()
{
	a = x[0];
	b = x[m-1];
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
 * Calculul coordonatei x pentru zona de afisat.
 */
int virtualX(double x)
{
	double d = (double) (getmaxx()-1)/(b-a);
	d = (double) d*(x-a);
	return round(d);
}

/*
 * Calculul coordonatei y pentru zona de afisat.
 */
int virtualY(double x)
{
	double d = (double) (getmaxy()-1)/(maxg-ming);
	d = (double) d*(x-ming);
	d = (double) (getmaxy()-1)-d;
	return round(d);
}

/*
 * Scalarea functiei reale la zona de afisat.
 */
void scale_function()
{
	for (int i = 0; i < m; i++)
	{
		x[i] = virtualX(x[i]);
		for (int j = 0; j < n; j++)
			y[i][j] = virtualY(y[i][j]);
	}
}

void draw_function()
{
	char temp[20];

	setcolor(YELLOW);
	outtextxy(47, 10, text[n+1]);
	itoa(ming, text[0], 10);
	strcpy(temp, "Minim: ");
	strcat(temp, text[0]);
	outtextxy(47, 7+20*(n+1), temp);
	itoa(maxg, text[0], 10);
	strcpy(temp, "Maxim: ");
	strcat(temp, text[0]);
	outtextxy(47, 7+20*(n+2), temp);

	for (int i = 0; i < n; i++)
	{
		setcolor(color[i]);
		line(10, 10+20*(i+1), 40, 10+20*(i+1));
		outtextxy(47, 7+20*(i+1), text[i+1]);

		moveto(x[0], y[0][i]);

		for (int j = 1; j < m; j++)
			lineto(x[j], y[j][i]);
	}
}

void main(int argc, char *argv[])
{
	FILE *f;

	if (argc != 3)
	{
		printf("\n\tUsage: grafic.exe <file_name> <nr_of_graphs>");
		getch();
		exit(1);
	}

	if ((f = fopen(argv[1], "r")) == NULL)
	{
		printf("Couldn't open file: %s", argv[1]);
		getch();
		exit(1);
	}

	n = atoi(argv[2]);
	if ((n > 10) || (n < 1))
	{
		printf("The number of graphs must be a value between 1 and 10");
		getch();
		exit(1);
	}

	for (int i = 0; i <= n; i++)
		fscanf(f, "%s", text[i+1]);

	while (!feof(f))
	{
		m++;
		fscanf(f, "%d", &x[m]);
		for (i = 0; i < n; i++)
		{
			fscanf(f, "%d", &y[m][i]);
		}
		color[m] = m+1;
	}
	m++;
	fclose(f);

	ming = compute_min();
	maxg = compute_max();

	int gd = DETECT, gm;
	initgraph(&gd, &gm, BGI);

	initialize_a_b();
	scale_function();
	draw_function();

	char c = getch();
	if (c == 0)
		getch();

	closegraph();
}