/*
 * Implementation file for geometrical figures library.
 *
 * Author: Bogdan DUMITRIU
 * Date:   01.12.2001
 */

#include "figures.h"

/*
 * Returns 1, 2 or 3 if m1, m2 and m3 respectively are the
 * smallest.
 */
int min(double m1, double m2, double m3)
{
	if (m2 <= m1)
		if (m2 <= m3)
			return 2;
		else
			return 3;
	else
		if (m1 <= m3)
			return 1;
		else
			return 3;
}

/*
 * Returns 1 if point (xx,yy) is on line from (x1,y1) to (x2,y2)
 * and 0 otherwise.
 */
int pointOnLine(int xx, int yy, int x1, int y1, int x2, int y2)
{
	int x, y, xc, yc;

	int dx = abs(x2-x1);
	int dy = abs(y2-y1);

	int caz = 0;

	if (((x1-x2) <= 0) && ((y1-y2) <= 0))
		caz = 1;
	if (((x1-x2) > 0) && ((y1-y2) > 0))
		caz = 1;

	if (dx >= dy)
	{
		int d = 2*dy-dx;
		int inc1 = 2*dy;
		int inc2 = 2*(dy-dx);
		if (x1 > x2)
		{
			x = x2;
			y = y2;
			xc = x1;
		}
		else
		{
			x = x1;
			y = y1;
			xc = x2;
		}
		while (x < xc)
		{
			if ((abs(x-xx) <= 1) && (abs(y-yy) <= 1))
				return 1;
			x++;
			if (d < 0)
				d = d+inc1;
			else
			{
				if (caz)
					y++;
				else
					y--;
				d = d+inc2;
			}
		}
	}
	else
	{
		int d = 2*dx-dy;
		int inc1 = 2*dx;
		int inc2 = 2*(dx-dy);
		if (y1 > y2)
		{
			x = x2;
			y = y2;
			yc = y1;
		}
		else
		{
			x = x1;
			y = y1;
			yc = y2;
		}
		while (y < yc)
		{
			if ((abs(x-xx) <= 1) && (abs(y-yy) <= 1))
				return 1;
			y++;
			if (d < 0)
				d = d+inc1;
			else
			{
				if (caz)
					x++;
				else
					x--;
				d = d+inc2;
			}
		}
	}
	return 0;
}

/*
 * Returns 1 if point (xx,yy) is on the circle with center
 * in (cx,cy) & with radius of R and 0 otherwise.
 */
int pointOnCircle(int xx, int yy, int cx, int cy, int R)
{
	int x = 0;
	int y = R;
	int d1, d2;
	int D = 2-2*R;

	do
	{
		if ((abs(xx-cx-x) <= 1) && (abs(yy-cy-y) <= 1))
			return 1;
		if ((abs(xx-cx+x) <= 1) && (abs(yy-cy-y) <= 1))
			return 1;
		if ((abs(xx-cx+x) <= 1) && (abs(yy-cy+y) <= 1))
			return 1;
		if ((abs(xx-cx-x) <= 1) && (abs(yy-cy+y) <= 1))
			return 1;

		d1 = 2*D+2*y-1;
		d2 = 2*D-2*x-1;

		if ((D < 0) && (d1 <= 0))
		{
			x++;
			D = D+2*x+1;
		}
		else
			if (((D < 0) && (d1 > 0)) || ((D > 0) && (d2 <= 0)) || (D == 0))
			{
				x++;
				y--;
				D = D+2*x-2*y+2;
			}
			else
				if ((D > 0) && (d2 > 0))
				{
					y--;
					D = D-2*y+1;
				}
	}
	while (y > 0);
	return 0;
}

int pointOnEllipse(int xx, int yy, int cx, int cy, int rx, int ry)
{
	int x = rx;
	int y = 0;

	do
	{
		if ((abs(xx-cx-x) <= 1) && (abs(yy-cy-y) <= 1))
			return 1;
		if ((abs(xx-cx+x) <= 1) && (abs(yy-cy-y) <= 1))
			return 1;
		if ((abs(xx-cx+x) <= 1) && (abs(yy-cy+y) <= 1))
			return 1;
		if ((abs(xx-cx-x) <= 1) && (abs(yy-cy+y) <= 1))
			return 1;

		double t1 = (double) ((double) x*x)/((double) rx*rx);
		double t2 = (double) ((double) (x-1)*(x-1))/((double) rx*rx);
		double t3 = (double) ((double) y*y)/((double) ry*ry);
		double t4 = (double) ((double) (y-1)*(y-1))/((double) ry*ry);

		double m1 = fabs(t2+t3-1);
		double m2 = fabs(t2+t4-1);
		double m3 = fabs(t1+t4-1);

		switch (min(m1, m2, m3))
		{
			case 1:
				x--;
				break;
			case 2:
				x--;
				y--;
				break;
			case 3:
				y--;
				break;
		}
	}
	while (x >= 0);
	return 0;
}

void Line::draw()
{
	if (selected)
		setcolor(SEL_COLOR);
	else
		setcolor(color);
	line(x1, y1, x2, y2);
}

void Line::drawBlack()
{
	setcolor(BLACK);
	line(x1, y1, x2, y2);
}

void Line::writeToFile(FILE *f)
{
	fprintf(f, "%d %d %d %d %d %d\n", LINE_ID, color, x1, y1, x2, y2);
}

int Line::contains(int x, int y)
{
	return pointOnLine(x, y, x1, y1, x2, y2);
}

Polyline::Polyline(int n, int *points, int color)
: Figure(color)
{
	this->n = 2*n;
	this->points = (int*) malloc(this->n*sizeof(int));
	for (int i = 0; i < this->n; i++)
		this->points[i] = points[i];
}

Polyline::Polyline(int data[200])
: Figure(data[0])
{
	n = data[1];
	points = (int*) malloc(this->n*sizeof(int));
	for (int i = 0; i < n; i++)
		points[i] = data[i+2];
}

void Polyline::draw()
{
	if (selected)
		setcolor(SEL_COLOR);
	else
		setcolor(color);
	for (int i = 0; i < n-3; i += 2)
		line(points[i], points[i+1], points[i+2], points[i+3]);
}

void Polyline::drawBlack()
{
	setcolor(BLACK);
	for (int i = 0; i < n-3; i += 2)
		line(points[i], points[i+1], points[i+2], points[i+3]);
}

void Polyline::writeToFile(FILE *f)
{
	fprintf(f, "%d %d %d", POLYLINE_ID, color, n);
	for (int i = 0; i < n; i++)
		fprintf(f, " %d", points[i]);
	fprintf(f, "\n");
}

int Polyline::contains(int x, int y)
{
	for (int i = 0; i < n-3; i += 2)
		if (pointOnLine(x, y,
			points[i], points[i+1], points[i+2], points[i+3]))
			return 1;
	return 0;
}

void Polyline::move(int x, int y)
{
	for (int i = 0; i < n; i++)
		if (i%2 == 0)
			points[i] += x;
		else
			points[i] += y;
}

void Circle::draw()
{
	if (selected)
		setcolor(SEL_COLOR);
	else
		setcolor(color);
	circle(x, y, radius);
}

void Circle::drawBlack()
{
	setcolor(BLACK);
	circle(x, y, radius);
}

void Circle::writeToFile(FILE *f)
{
	fprintf(f, "%d %d %d %d %d\n", CIRCLE_ID, color, x, y, radius);
}

int Circle::contains(int xx, int yy)
{
	return pointOnCircle(xx, yy, x, y, radius);
}

void Ellipse::draw()
{
	if (selected)
		setcolor(SEL_COLOR);
	else
		setcolor(color);
	ellipse(x, y, 0, 360, rx, ry);
}

void Ellipse::drawBlack()
{
	setcolor(BLACK);
	ellipse(x, y, 0, 360, rx, ry);
}

void Ellipse::writeToFile(FILE *f)
{
	fprintf(f, "%d %d %d %d %d %d\n", ELLIPSE_ID, color, x, y, rx, ry);
}

int Ellipse::contains(int xx, int yy)
{
	return pointOnEllipse(xx, yy, x, y, rx, ry);
}

void Rectangle::draw()
{
	if (selected)
		setcolor(SEL_COLOR);
	else
		setcolor(color);
	rectangle(x1, y1, x2, y2);
}

void Rectangle::drawBlack()
{
	setcolor(BLACK);
	rectangle(x1, y1, x2, y2);
}

void Rectangle::writeToFile(FILE *f)
{
	fprintf(f, "%d %d %d %d %d %d\n", RECTANGLE_ID, color,
		x1, y1, x2, y2);
}

int Rectangle::contains(int x, int y)
{
	return (pointOnLine(x, y, x1, y1, x1, y2) ||
		pointOnLine(x, y, x1, y2, x2, y2) ||
		pointOnLine(x, y, x2, y2, x2, y1) ||
		pointOnLine(x, y, x2, y1, x1, y1));
}

void FilledCircle::draw()
{
	setfillstyle(SOLID_FILL, fcol);
	if (selected)
		setcolor(SEL_COLOR);
	else
		setcolor(color);
	fillellipse(x, y, radius, radius);
}

void FilledCircle::drawBlack()
{
	setfillstyle(SOLID_FILL, BLACK);
	setcolor(BLACK);
	fillellipse(x, y, radius, radius);
}

void FilledCircle::writeToFile(FILE *f)
{
	fprintf(f, "%d %d %d %d %d %d\n", FILLED_CIRCLE_ID, color, fcol,
		x, y, radius);
}

void FilledEllipse::draw()
{
	setfillstyle(SOLID_FILL, fcol);
	if (selected)
		setcolor(SEL_COLOR);
	else
		setcolor(color);
	fillellipse(x, y, rx, ry);
}

void FilledEllipse::drawBlack()
{
	setfillstyle(SOLID_FILL, BLACK);
	setcolor(BLACK);
	fillellipse(x, y, rx, ry);
}

void FilledEllipse::writeToFile(FILE *f)
{
	fprintf(f, "%d %d %d %d %d %d %d\n", FILLED_ELLIPSE_ID, color, fcol,
		x, y, rx, ry);
}

void FilledRectangle::draw()
{
	setfillstyle(SOLID_FILL, fcol);
	if (selected)
		setcolor(SEL_COLOR);
	else
		setcolor(color);
	bar3d(x1, y1, x2, y2, 0, 0);
}

void FilledRectangle::drawBlack()
{
	setfillstyle(SOLID_FILL, BLACK);
	setcolor(BLACK);
	bar3d(x1, y1, x2, y2, 0, 0);
}

void FilledRectangle::writeToFile(FILE *f)
{
	fprintf(f, "%d %d %d %d %d %d %d\n", FILLED_RECTANGLE_ID, color, fcol,
		x1, y1, x2, y2);
}