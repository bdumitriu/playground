/*
 * Implementation for the workarea class.
 *
 * Author: Bogdan DUMITRIU
 * Date:   02.12.2001
 */

#include <stdlib.h>
#include "workarea.h"

WorkArea::WorkArea(int x1, int y1, int x2, int y2, int backgroundColor,
		Colors *colors)
: x1(x1), y1(y1), x2(x2), y2(y2), bgCol(backgroundColor), col(colors)
{
	action = 0;
	items = new List();
}

WorkArea::~WorkArea()
{
	delete items;
}

void WorkArea::draw()
{
	struct viewporttype vp;
	getviewsettings(&vp);
	setviewport(x1, y1, x2, y2, 1);
	setfillstyle(SOLID_FILL, bgCol);
	bar(x1, y1, x2, y2);
	items->drawAll();
	setviewport(vp.left, vp.top, vp.right, vp.bottom, vp.clip);
}

void WorkArea::deselect()
{
	items->deselectAll();
}

int WorkArea::contains(int x, int y)
{
	return ((x >= x1) && (x <= x2) && (y >= y1) && (y <= y2));
}

void WorkArea::handleMouse()
{
	POINT pos;
	int button;
	struct viewporttype vp;

	getviewsettings(&vp);
	setviewport(x1, y1, x2, y2, 1);

	button = mouse_status(&pos);
	if (!contains(pos.x, pos.y))
		return;
	while (contains(pos.x, pos.y))
	{
		switch (action)
		{
			case 0:		// object selection/movement/deletion
				if (button == 1)
				{
					int xs1 = pos.x;
					int ys1 = pos.y;
					int tst = 1;
					do
					{
						if ((xs1 != pos.x) || (ys1 != pos.y))
						{
							tst = 0;		// we do movement
							mouse_off();
							items->drawAllBlack();
							items->moveAllSelected(pos.x-xs1, pos.y-ys1);
							items->drawAll();
							mouse_on();
							xs1 = pos.x;
							ys1 = pos.y;
						}
						button = mouse_status(&pos);
					}
					while (button == 1);
					if (tst)				// we do status change
					{
						items->changeStatus(pos.x-x1, pos.y-y1);
						mouse_off();
						items->drawAll();
						mouse_on();
					}
				}
				else
				{
					if (kbhit())
					{
						if (getch() == 0)
							if (getch() == 83)
							{
								mouse_off();
								items->drawAllBlack();
								items->removeAllSelected();
								items->drawAll();
								mouse_on();
							}
					}
				}
				break;
			case 1:		// handles line drawing
				if (button == 1)
				{
					int xl1 = pos.x-x1, yl1 = pos.y-y1;
					int xl2 = xl1, yl2 = yl1;

					setwritemode(XOR_PUT);
					setcolor(col->getFgColor());
					mouse_off();
					line(xl1, yl1, xl2, yl2);
					mouse_on();
					mouse_restrict(x1, y1, x2, y2);
					mouse_wait_for_release(1, &pos);
					do
					{
						button = mouse_status(&pos);
						if ((pos.x-x1 != xl2) || (pos.y-y1 != yl2))
						{
							mouse_off();
							line(xl1, yl1, xl2, yl2);
							xl2 = pos.x-x1;
							yl2 = pos.y-y1;
							line(xl1, yl1, xl2, yl2);
							mouse_on();
						}
					}
					while (button != 1);
					mouse_wait_for_release(1, &pos);
					mouse_free(getmaxx(), getmaxy());
					setwritemode(COPY_PUT);
					Line *newLine = new Line(xl1, yl1, xl2, yl2,
						col->getFgColor());
					items->add(new Node(newLine));
					mouse_off();
					items->drawAll();
					mouse_on();
				}
				break;
			case 2:		// hadles polyline drawing
				if (button == 1)
				{
					int points[200];
					int idx = 0;

					points[idx++] = pos.x-x1;
					points[idx++] = pos.y-y1;
					int xl = pos.x-x1, yl = pos.y-y1;

					setwritemode(XOR_PUT);
					setcolor(col->getFgColor());
					mouse_restrict(x1, y1, x2, y2);
					mouse_wait_for_release(1, &pos);
					do
					{
						do
						{
							button = mouse_status(&pos);
							if ((pos.x-x1 != xl) || (pos.y-y1 != yl))
							{
								mouse_off();
								line(points[idx-2], points[idx-1],
									xl, yl);
								xl = pos.x-x1;
								yl = pos.y-y1;
								line(points[idx-2], points[idx-1],
									xl, yl);
								mouse_on();
							}
						}
						while ((button != 1) && (button != 2));
						mouse_wait_for_release(button, &pos);
						points[idx++] = xl;
						points[idx++] = yl;
					}
					while (button != 2);
					mouse_wait_for_release(2, &pos);
					mouse_free(getmaxx(), getmaxy());
					setwritemode(COPY_PUT);
					Polyline *newPolyLine = new Polyline(idx/2, points,
						col->getFgColor());
					items->add(new Node(newPolyLine));
					mouse_off();
					items->drawAll();
					mouse_on();
				}
				break;
			case 3:		// hadles polygon drawing
				if (button == 1)
				{
					int points[200];
					int idx = 0;

					points[idx++] = pos.x-x1;
					points[idx++] = pos.y-y1;
					int xl = pos.x-x1, yl = pos.y-y1;

					setwritemode(XOR_PUT);
					setcolor(col->getFgColor());
					mouse_restrict(x1, y1, x2, y2);
					mouse_wait_for_release(1, &pos);
					do
					{
						do
						{
							button = mouse_status(&pos);
							if ((pos.x-x1 != xl) || (pos.y-y1 != yl))
							{
								mouse_off();
								line(points[idx-2], points[idx-1],
									xl, yl);
								xl = pos.x-x1;
								yl = pos.y-y1;
								line(points[idx-2], points[idx-1],
									xl, yl);
								mouse_on();
							}
						}
						while ((button != 1) && (button != 2));
						mouse_wait_for_release(button, &pos);
						points[idx++] = xl;
						points[idx++] = yl;
					}
					while (button != 2);
					mouse_wait_for_release(2, &pos);
					points[idx++] = points[0];
					points[idx++] = points[1];
					mouse_free(getmaxx(), getmaxy());
					setwritemode(COPY_PUT);
					Polyline *newPolyLine = new Polyline(idx/2, points,
						col->getFgColor());
					items->add(new Node(newPolyLine));
					mouse_off();
					items->drawAll();
					mouse_on();
				}
				break;
			case 4:		// hadles circle drawing
				if (button == 1)
				{
					int xl1 = pos.x-x1, yl1 = pos.y-y1;
					int xl2 = xl1, yl2 = yl1;

					setcolor(col->getFgColor());
					mouse_off();
					int radius;
					if (abs(xl1-xl2)/2 < abs(yl1-yl2)/2)
						radius = abs(yl1-yl2)/2;
					else
						radius = abs(xl1-xl2)/2;
					circle((xl1+xl2)/2, (yl1+yl2)/2, radius);
					mouse_on();
					mouse_restrict(x1, y1, x2, y2);
					mouse_wait_for_release(1, &pos);
					do
					{
						button = mouse_status(&pos);
						if ((pos.x-x1 != xl2) || (pos.y-y1 != yl2))
						{
							mouse_set_position(&pos);
							mouse_off();
							setcolor(BLACK);
							if (abs(xl1-xl2)/2 < abs(yl1-yl2)/2)
								radius = abs(yl1-yl2)/2;
							else
								radius = abs(xl1-xl2)/2;
							circle((xl1+xl2)/2, (yl1+yl2)/2, radius);
							items->drawAll();
							setcolor(col->getFgColor());
							xl2 = pos.x-x1;
							yl2 = pos.y-y1;
							if (abs(xl1-xl2)/2 < abs(yl1-yl2)/2)
								radius = abs(yl1-yl2)/2;
							else
								radius = abs(xl1-xl2)/2;
							circle((xl1+xl2)/2, (yl1+yl2)/2, radius);
							mouse_on();
						}
					}
					while (button != 1);
					mouse_wait_for_release(1, &pos);
					mouse_free(getmaxx(), getmaxy());
					if (abs(xl1-xl2)/2 < abs(yl1-yl2)/2)
						radius = abs(yl1-yl2)/2;
					else
						radius = abs(xl1-xl2)/2;
					Circle *newCircle = new Circle((xl1+xl2)/2, (yl1+yl2)/2,
						radius, col->getFgColor());
					items->add(new Node(newCircle));
					mouse_off();
					items->drawAll();
					mouse_on();
				}
				break;
			case 5:		// hadles filled circle drawing
				if (button == 1)
				{
					int xl1 = pos.x-x1, yl1 = pos.y-y1;
					int xl2 = xl1, yl2 = yl1;

					setcolor(col->getFgColor());
					mouse_off();
					int radius;
					if (abs(xl1-xl2)/2 < abs(yl1-yl2)/2)
						radius = abs(yl1-yl2)/2;
					else
						radius = abs(xl1-xl2)/2;
					circle((xl1+xl2)/2, (yl1+yl2)/2, radius);
					mouse_on();
					mouse_restrict(x1, y1, x2, y2);
					mouse_wait_for_release(1, &pos);
					do
					{
						button = mouse_status(&pos);
						if ((pos.x-x1 != xl2) || (pos.y-y1 != yl2))
						{
							mouse_set_position(&pos);
							mouse_off();
							setcolor(BLACK);
							if (abs(xl1-xl2)/2 < abs(yl1-yl2)/2)
								radius = abs(yl1-yl2)/2;
							else
								radius = abs(xl1-xl2)/2;
							circle((xl1+xl2)/2, (yl1+yl2)/2, radius);
							items->drawAll();
							setcolor(col->getFgColor());
							xl2 = pos.x-x1;
							yl2 = pos.y-y1;
							if (abs(xl1-xl2)/2 < abs(yl1-yl2)/2)
								radius = abs(yl1-yl2)/2;
							else
								radius = abs(xl1-xl2)/2;
							circle((xl1+xl2)/2, (yl1+yl2)/2, radius);
							mouse_on();
						}
					}
					while (button != 1);
					mouse_wait_for_release(1, &pos);
					mouse_free(getmaxx(), getmaxy());
					if (abs(xl1-xl2)/2 < abs(yl1-yl2)/2)
						radius = abs(yl1-yl2)/2;
					else
						radius = abs(xl1-xl2)/2;
					FilledCircle *newFilledCircle = new FilledCircle(
						(xl1+xl2)/2, (yl1+yl2)/2, radius,
						col->getFgColor(), col->getBgColor());
					items->add(new Node(newFilledCircle));
					mouse_off();
					items->drawAll();
					mouse_on();
				}
				break;
			case 6:		// hadles ellipse drawing
				if (button == 1)
				{
					int xl1 = pos.x-x1, yl1 = pos.y-y1;
					int xl2 = xl1, yl2 = yl1;

					setcolor(col->getFgColor());
					mouse_off();
					ellipse((xl1+xl2)/2, (yl1+yl2)/2, 0, 360,
						abs(xl1-xl2)/2, abs(yl1-yl2));
					mouse_on();
					mouse_restrict(x1, y1, x2, y2);
					mouse_wait_for_release(1, &pos);
					do
					{
						button = mouse_status(&pos);
						if ((pos.x-x1 != xl2) || (pos.y-y1 != yl2))
						{
							mouse_set_position(&pos);
							mouse_off();
							setcolor(BLACK);
							ellipse((xl1+xl2)/2, (yl1+yl2)/2, 0, 360,
								abs(xl1-xl2)/2, abs(yl1-yl2));
							items->drawAll();
							setcolor(col->getFgColor());
							xl2 = pos.x-x1;
							yl2 = pos.y-y1;
							ellipse((xl1+xl2)/2, (yl1+yl2)/2, 0, 360,
								abs(xl1-xl2)/2, abs(yl1-yl2));
							mouse_on();
						}
					}
					while (button != 1);
					mouse_wait_for_release(1, &pos);
					mouse_free(getmaxx(), getmaxy());
					Ellipse *newEllipse = new Ellipse((xl1+xl2)/2,
						(yl1+yl2)/2, abs(xl1-xl2)/2,
						abs(yl1-yl2), col->getFgColor());
					items->add(new Node(newEllipse));
					mouse_off();
					items->drawAll();
					mouse_on();
				}
				break;
			case 7:		// hadles filled ellipse drawing
				if (button == 1)
				{
					int xl1 = pos.x-x1, yl1 = pos.y-y1;
					int xl2 = xl1, yl2 = yl1;

					setcolor(col->getFgColor());
					mouse_off();
					ellipse((xl1+xl2)/2, (yl1+yl2)/2, 0, 360,
						abs(xl1-xl2)/2, abs(yl1-yl2));
					mouse_on();
					mouse_restrict(x1, y1, x2, y2);
					mouse_wait_for_release(1, &pos);
					do
					{
						button = mouse_status(&pos);
						if ((pos.x-x1 != xl2) || (pos.y-y1 != yl2))
						{
							mouse_set_position(&pos);
							mouse_off();
							setcolor(BLACK);
							ellipse((xl1+xl2)/2, (yl1+yl2)/2, 0, 360,
								abs(xl1-xl2)/2, abs(yl1-yl2));
							items->drawAll();
							setcolor(col->getFgColor());
							xl2 = pos.x-x1;
							yl2 = pos.y-y1;
							ellipse((xl1+xl2)/2, (yl1+yl2)/2, 0, 360,
								abs(xl1-xl2)/2, abs(yl1-yl2));
							mouse_on();
						}
					}
					while (button != 1);
					mouse_wait_for_release(1, &pos);
					mouse_free(getmaxx(), getmaxy());
					FilledEllipse *newFilledEllipse = new FilledEllipse(
						(xl1+xl2)/2, (yl1+yl2)/2, abs(xl1-xl2)/2,
						abs(yl1-yl2), col->getFgColor(), col->getBgColor());
					items->add(new Node(newFilledEllipse));
					mouse_off();
					items->drawAll();
					mouse_on();
				}
				break;
			case 8:		// handles rectangle drawing
				if (button == 1)
				{
					int xl1 = pos.x-x1, yl1 = pos.y-y1;
					int xl2 = xl1, yl2 = yl1;

					setwritemode(XOR_PUT);
					setcolor(col->getFgColor());
					mouse_off();
					rectangle(xl1, yl1, xl2, yl2);
					mouse_on();
					mouse_restrict(x1, y1, x2, y2);
					mouse_wait_for_release(1, &pos);
					do
					{
						button = mouse_status(&pos);
						if ((pos.x-x1 != xl2) || (pos.y-y1 != yl2))
						{
							mouse_off();
							rectangle(xl1, yl1, xl2, yl2);
							xl2 = pos.x-x1;
							yl2 = pos.y-y1;
							rectangle(xl1, yl1, xl2, yl2);
							mouse_on();
						}
					}
					while (button != 1);
					mouse_wait_for_release(1, &pos);
					mouse_free(getmaxx(), getmaxy());
					setwritemode(COPY_PUT);
					Rectangle *newRectangle = new Rectangle(xl1, yl1,
						xl2, yl2, col->getFgColor());
					items->add(new Node(newRectangle));
					mouse_off();
					items->drawAll();
					mouse_on();
				}
				break;
			case 9:		// handles rectangle drawing
				if (button == 1)
				{
					int xl1 = pos.x-x1, yl1 = pos.y-y1;
					int xl2 = xl1, yl2 = yl1;

					setwritemode(XOR_PUT);
					setcolor(col->getFgColor());
					mouse_off();
					rectangle(xl1, yl1, xl2, yl2);
					mouse_on();
					mouse_restrict(x1, y1, x2, y2);
					mouse_wait_for_release(1, &pos);
					do
					{
						button = mouse_status(&pos);
						if ((pos.x-x1 != xl2) || (pos.y-y1 != yl2))
						{
							mouse_off();
							rectangle(xl1, yl1, xl2, yl2);
							xl2 = pos.x-x1;
							yl2 = pos.y-y1;
							rectangle(xl1, yl1, xl2, yl2);
							mouse_on();
						}
					}
					while (button != 1);
					mouse_wait_for_release(1, &pos);
					mouse_free(getmaxx(), getmaxy());
					setwritemode(COPY_PUT);
					FilledRectangle *newFilledRectangle =
						new FilledRectangle(xl1, yl1, xl2, yl2,
						col->getFgColor(), col->getBgColor());
					items->add(new Node(newFilledRectangle));
					mouse_off();
					items->drawAll();
					mouse_on();
				}
				break;
		}
		button = mouse_status(&pos);
	}
	setviewport(vp.left, vp.top, vp.right, vp.bottom, vp.clip);
}

void WorkArea::save(FILE *f)
{
	items->writeAll(f);
}

void WorkArea::load(FILE *f)
{
	delete items;
	struct viewporttype vp;

	getviewsettings(&vp);
	setviewport(x1, y1, x2, y2, 1);
	items = new List();
	clearviewport();

	int fig, i, a[200];
	char *stream = (char*) malloc(1000*sizeof(char));
	char *n, *temp;

	while (fgets(stream, 1000, f))
	{
		i = -1;
		temp = stream;
		while ((n = strtok(temp, " ")) != NULL)
		{
			if (i == -1)
			{
				i = 0;
				fig = atoi(n);
			}
			else
				a[i++] = atoi(n);
			temp = NULL;
		}
		switch (fig)
		{
			case LINE_ID:
				items->add(new Node(new Line(a)));
				break;
			case POLYLINE_ID:
				items->add(new Node(new Polyline(a)));
				break;
			case CIRCLE_ID:
				items->add(new Node(new Circle(a)));
				break;
			case ELLIPSE_ID:
				items->add(new Node(new Ellipse(a)));
				break;
			case RECTANGLE_ID:
				items->add(new Node(new Rectangle(a)));
				break;
			case FILLED_CIRCLE_ID:
				items->add(new Node(new FilledCircle(a)));
				break;
			case FILLED_ELLIPSE_ID:
				items->add(new Node(new FilledEllipse(a)));
				break;
			case FILLED_RECTANGLE_ID:
				items->add(new Node(new FilledRectangle(a)));
				break;
		}
	}

	free(stream);
	mouse_off();
	items->drawAll();
	mouse_on();
	setviewport(vp.left, vp.top, vp.right, vp.bottom, vp.clip);
}

void WorkArea::clear()
{
	delete items;
	struct viewporttype vp;

	getviewsettings(&vp);
	setviewport(x1, y1, x2, y2, 1);
	items = new List();
	clearviewport();
	setviewport(vp.left, vp.top, vp.right, vp.bottom, vp.clip);
}
