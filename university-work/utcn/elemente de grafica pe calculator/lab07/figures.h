/*
 * Header file for geometrical figures library.
 *
 * Author: Bogdan DUMITRIU
 * Date:   01.12.2001
 */

#include <stdio.h>
#include <graphics.h>
#include <alloc.h>
#include <math.h>

#ifndef __FIGURES_H_
#define __FIGURES_H_

/*
 * A set of ids for each figure type. These will be used
 * to identify a certain figure in a file, i.e. when one
 * of the figures writes itself in a file it will precede
 * its data by this id.
 */
#ifndef LINE_ID
#define LINE_ID 1
#endif

#ifndef POLYLINE_ID
#define POLYLINE_ID 2
#endif

#ifndef CIRCLE_ID
#define CIRCLE_ID 3
#endif

#ifndef ELLIPSE_ID
#define ELLIPSE_ID 4
#endif

#ifndef RECTANGLE_ID
#define RECTANGLE_ID 5
#endif

#ifndef FILLED_CIRCLE_ID
#define FILLED_CIRCLE_ID 6
#endif

#ifndef FILLED_ELLIPSE_ID
#define FILLED_ELLIPSE_ID 7
#endif

#ifndef FILLED_RECTANGLE_ID
#define FILLED_RECTANGLE_ID 8
#endif

/*
 * The color with which selected figures are drawn.
 */
#ifndef SEL_COLOR
#define SEL_COLOR 15
#endif

/*
 * The generic figure class. Objects of type Figure should not be
 * directly instantiated as this class has a very limited
 * functionality. It simply provides a base class for all the
 * other geometrical figures.
 */
class Figure
{
public:
	/*
	 * Builds a new Figure object that will draw itself with the
	 * with the specified color.
	 */
	Figure(int color = LIGHTGRAY)
	: color(color), selected(0)
	{}
	virtual ~Figure()
	{}

	/*
	 * Draws the figure on the screen.
	 */
	virtual void draw() = 0;
	/*
	 * Draws the figure on the screen with the black color.
	 */
	virtual void drawBlack() = 0;
	/*
	 * Writes all relevant data about this figure to the specified file
	 * after a given pattern.
	 */
	virtual void writeToFile(FILE *f)
	{}
	/*
	 * Returns 1 if figure contains point (x,y) and 0 otherwise.
	 */
	virtual int contains(int x, int y)
	{
		return 0;
	}
	/*
	 * Selects the current figure.
	 */
	virtual void select()
	{
		selected = 1;
	}
	/*
	 * Deselects the current figure.
	 */
	virtual void deselect()
	{
		selected = 0;
	}
	/*
	 * Returns 1 if figure is selected and 0 otherwise.
	 */
	virtual int getSelected()
	{
		return selected;
	}
	/*
	 * Tranlates the figure with the specified amount on each axis.
	 */
	virtual void move(int x, int y)
	{}
protected:
	int selected;
	int color;
};

/*
 * The class that handles simple lines.
 */
class Line: public Figure
{
public:
	/*
	 * Builds a new Line object from (x1,y1) to (x2,y2) with
	 * the specified color.
	 */
	Line(int x1, int y1, int x2, int y2, int color)
	: Figure(color), x1(x1), y1(y1), x2(x2), y2(y2)
	{}
	/*
	 * Builds a new Line from an array of integers.
	 * This constructor is useful when loading from a file.
	 * The sequence of numbers should match the pattern
	 * given in the description of the writeToFile method,
	 * from which the first element (the ID) should be
	 * missing.
	 */
	Line(int data[5])
	: Figure(data[0])
	{
		x1 = data[1];
		y1 = data[2];
		x2 = data[3];
		y2 = data[4];
	}
	~Line()
	{}

	/*
	 * Draws the line on the screen.
	 */
	void draw();
	/*
	 * Draws the figure on the screen with the black color.
	 */
	void drawBlack();
	/*
	 * Writes all relevant data about this line to the specified file
	 * after the following pattern:
	 * LINE_ID color x1 x2 y1 y2\n
	 */
	void writeToFile(FILE *f);
	/*
	 * Returns 1 if line contains point (x,y) and 0 otherwise.
	 */
	virtual int contains(int x, int y);
	/*
	 * Tranlates the line with the specified amount on each axis.
	 */
	void move(int x, int y)
	{
		x1 += x; y1 += y; x2 += x; y2 += y;
	}
private:
	int x1, y1;
	int x2, y2;
};

/*
 * The class that handles polylines. Polylines are defined
 * using a set of points with two adjacent points forming
 * a line.
 */
class Polyline: public Figure
{
public:
	/*
	 * Builds a new Polyline object from the first 2*n points
	 * from the points array. The even array elements
	 * (points[0], points[2],...) represent the x coordinates
	 * while the odd array elements (points[1], points[3],...)
	 * represent the repective y coordinates. Polyline has
	 * the specified color.
	 *
	 * If you need a polygon, make sure that:
	 * 	points[0]=points[2n-2] and
	 *	points[1]=points[2n-1].
	 */
	Polyline(int n, int *points, int color);
	/*
	 * Builds a new Polyline from an array of integers.
	 * This constructor is useful when loading from a file.
	 * The sequence of numbers should match the pattern
	 * given in the description of the writeToFile method,
	 * from which the first element (the ID) should be
	 * missing.
	 */
	Polyline(int data[200]);
	~Polyline()
	{
		delete [] points;
	}

	/*
	 * Draws the polyline on the screen.
	 */
	void draw();
	/*
	 * Draws the figure on the screen with the black color.
	 */
	void drawBlack();
	/*
	 * Writes all relevant data about this poly line to
	 * the specified file after the following pattern:
	 * POLYLINE_ID color length_of_points points[0] ... points[n-1]\n
	 */
	void writeToFile(FILE *f);
	/*
	 * Returns 1 if polyline contains point (x,y) and 0 otherwise.
	 */
	virtual int contains(int x, int y);
	/*
	 * Tranlates the polyline with the specified amount on each axis.
	 */
	void move(int x, int y);
private:
	int n;
	int *points;
};

/*
 * The class that handles circles.
 */
class Circle: public Figure
{
public:
	/*
	 * Builds a new Circle object with its center in
	 * (center_x,center_y) and with radius as its radius.
	 * The circle will have the specified color.
	 */
	Circle(int center_x, int center_y, int radius, int color)
	: Figure(color), x(center_x), y(center_y), radius(radius)
	{}
	/*
	 * Builds a new Circle from an array of integers.
	 * This constructor is useful when loading from a file.
	 * The sequence of numbers should match the pattern
	 * given in the description of the writeToFile method,
	 * from which the first element (the ID) should be
	 * missing.
	 */
	Circle(int data[4])
	: Figure(data[0])
	{
		x = data[1];
		y = data[2];
        radius = data[3];
	}
	~Circle()
	{}

	/*
	 * Draws the circle on the screen.
	 */
	virtual void draw();
	/*
	 * Draws the figure on the screen with the black color.
	 */
	void drawBlack();
	/*
	 * Writes all relevant data about this circle to the specified file
	 * after the following pattern:
	 * CIRCLE_ID color center_x center_y radius\n
	 */
	virtual void writeToFile(FILE *f);
	/*
	 * Returns 1 if circle contains point (x,y) and 0 otherwise.
	 */
	int contains(int x, int y);
	/*
	 * Tranlates the circle with the specified amount on each axis.
	 */
	void move(int x, int y)
	{
		this->x += x; this->y += y;
	}
protected:
	int x, y;
	int radius;
};

/*
 * The class that handles ellipses.
 */
class Ellipse: public Figure
{
public:
	/*
	 * Builds a new Ellipse object with its center in
	 * (center_x,center_y) and with radius_x as its x radius
	 * and radius_y as its y radius. The ellipse will have
	 * the specified color.
	 */
	Ellipse(int center_x, int center_y, int radius_x, int radius_y, int color)
	: Figure(color), x(center_x), y(center_y), rx(radius_x), ry(radius_y)
	{}
	/*
	 * Builds a new Ellipse from an array of integers.
	 * This constructor is useful when loading from a file.
	 * The sequence of numbers should match the pattern
	 * given in the description of the writeToFile method,
	 * from which the first element (the ID) should be
	 * missing.
	 */
	Ellipse(int data[5])
	: Figure(data[0])
	{
		x = data[1];
		y = data[2];
		rx = data[3];
		ry = data[4];
	}
	~Ellipse()
	{}

	/*
	 * Draws the ellipse on the screen.
	 */
	/*
	 * Draws the figure on the screen with the black color.
	 */
	void drawBlack();
	virtual void draw();
	/*
	 * Writes all relevant data about this ellipse to the specified file
	 * after the following pattern:
	 * ELLIPSE_ID color center_x center_y radius_x radius_y\n
	 */
	virtual void writeToFile(FILE *f);
	/*
	 * Returns 1 if ellipse contains point (x,y) and 0 otherwise.
	 */
	int contains(int x, int y);
	/*
	 * Tranlates the ellipse with the specified amount on each axis.
	 */
	void move(int x, int y)
	{
		this->x += x; this->y += y;
	}
protected:
	int x, y;
	int rx, ry;
};

/*
 * The class that handles rectangles.
 */
class Rectangle: public Figure
{
public:
	/*
	 * Builds a new Rectangle object with left corner in
	 * (x1,y1) and right corner in (x2,y2) with the specified
	 * color.
	 */
	Rectangle(int x1, int y1, int x2, int y2, int color)
	: Figure(color), x1(x1), y1(y1), x2(x2), y2(y2)
	{}
	/*
	 * Builds a new Rectangle from an array of integers.
	 * This constructor is useful when loading from a file.
	 * The sequence of numbers should match the pattern
	 * given in the description of the writeToFile method,
	 * from which the first element (the ID) should be
	 * missing.
	 */
	Rectangle(int data[5])
	: Figure(data[0])
	{
		x1 = data[1];
		y1 = data[2];
		x2 = data[3];
		y2 = data[4];
	}
	~Rectangle()
	{}

	/*
	 * Draws the rectangle on the screen.
	 */
	virtual void draw();
	/*
	 * Draws the figure on the screen with the black color.
	 */
	void drawBlack();
	/*
	 * Writes all relevant data about this rectangle to the
	 * specified file after the following pattern:
	 * RECTANGLE_ID color x1 x2 y1 y2\n
	 */
	virtual void writeToFile(FILE *f);
	/*
	 * Returns 1 if rectangle contains point (x,y) and 0 otherwise.
	 */
	int contains(int x, int y);
	/*
	 * Tranlates the rectangle with the specified amount on each axis.
	 */
	void move(int x, int y)
	{
		x1 += x; y1 += y; x2 += x; y2 += y;
	}
protected:
	int x1, y1;
	int x2, y2;
};

/*
 * The class that handles filled circles.
 */
class FilledCircle: public Circle
{
public:
	/*
	 * Builds a new FilledCircle object with its center in
	 * (center_x,center_y) and with radius as its radius.
	 * The circle will have the specified color and the
	 * specified fill color.
	 */
	FilledCircle(int center_x, int center_y, int radius, int color,
		int fill_color)
	: Circle(center_x, center_y, radius, color), fcol(fill_color)
	{}
	/*
	 * Builds a new FilledCircle from an array of integers.
	 * This constructor is useful when loading from a file.
	 * The sequence of numbers should match the pattern
	 * given in the description of the writeToFile method,
	 * from which the first element (the ID) should be
	 * missing.
	 */
	FilledCircle(int data[5])
	: Circle(data[2], data[3], data[4], data[0])
	{
		fcol = data[1];
	}
	~FilledCircle()
	{}

	/*
	 * Draws the filled circle on the screen.
	 */
	void draw();
	/*
	 * Draws the figure on the screen with the black color.
	 */
	void drawBlack();
	/*
	 * Writes all relevant data about this filled circle to
	 * the specified file after the following pattern:
	 * FILLED_CIRCLE_ID color fill_color center_x center_y radius\n
	 */
	void writeToFile(FILE *f);
private:
	int fcol;
};

/*
 * The class that handles filled ellipses.
 */
class FilledEllipse: public Ellipse
{
public:
	/*
	 * Builds a new FilledEllipse object with its center in
	 * (center_x,center_y) and with radius_x as its x radius
	 * and radius_y as its y radius. The ellipse will have
	 * the specified color and the specified fill color.
	 */
	FilledEllipse(int center_x, int center_y, int radius_x, int radius_y,
		int color, int fill_color)
	: Ellipse(center_x, center_y, radius_x, radius_y, color),
		fcol(fill_color)
	{}
	/*
	 * Builds a new FilledEllipse from an array of integers.
	 * This constructor is useful when loading from a file.
	 * The sequence of numbers should match the pattern
	 * given in the description of the writeToFile method,
	 * from which the first element (the ID) should be
	 * missing.
	 */
	FilledEllipse(int data[6])
	: Ellipse(data[2], data[3], data[4], data[5], data[0])
	{
		fcol = data[1];
	}
	~FilledEllipse()
	{}

	/*
	 * Draws the filled ellipse on the screen.
	 */
	void draw();
	/*
	 * Draws the figure on the screen with the black color.
	 */
	void drawBlack();
	/*
	 * Writes all relevant data about this filled ellipse to
	 * the specified file after the following pattern:
	 * FILLED_ELLIPSE_ID color fill_color
	 * 	center_x center_y radius_x radius_y\n
	 */
	void writeToFile(FILE *f);
private:
	int fcol;
};

/*
 * The class that handles filled rectangles.
 */
class FilledRectangle: public Rectangle
{
public:
	/*
	 * Builds a new FilledRectangle object with left corner in
	 * (x1,y1) and right corner in (x2,y2) with the specified
	 * color.
	 */
	FilledRectangle(int x1, int y1, int x2, int y2, int color,
		int fill_color)
	: Rectangle(x1, y1, x2, y2, color), fcol(fill_color)
	{}
	/*
	 * Builds a new FilledRectangle from an array of integers.
	 * This constructor is useful when loading from a file.
	 * The sequence of numbers should match the pattern
	 * given in the description of the writeToFile method,
	 * from which the first element (the ID) should be
	 * missing.
	 */
	FilledRectangle(int data[6])
	: Rectangle(data[2], data[3], data[4], data[5], data[0])
	{
		fcol = data[1];
	}
	~FilledRectangle()
	{}

	/*
	 * Draws the filled rectangle on the screen.
	 */
	void draw();
	/*
	 * Draws the figure on the screen with the black color.
	 */
	void drawBlack();
	/*
	 * Writes all relevant data about this filled rectangle to the
	 * specified file after the following pattern:
	 * FILLED_RECTANGLE_ID color fill_color x1 x2 y1 y2\n
	 */
	void writeToFile(FILE *f);
private:
	int fcol;
};

#endif