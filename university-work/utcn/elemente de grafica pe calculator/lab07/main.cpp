#include <stdio.h>
#include <conio.h>
#include "manager.h"

void main()
{
	/* request auto detection */
	int gdriver = DETECT, gmode, errorcode;

	/* initialize graphics mode */
	initgraph(&gdriver, &gmode, "c:\\bc31\\bgi");

/*	int a[12] = {10, 10, 20, 30, 110, 150, 20, 200, 10, 130, 10, 10};

	Figure *l = new Line(10, 100, 200, 305, CYAN);
	Polyline *pl = new Polyline(6, a, MAGENTA);
	Circle *c = new Circle(100, 200, 30, RED);
	Ellipse *e = new Ellipse(300, 300, 100, 50, BROWN);
	Figure *r = new Rectangle(150, 150, 500, 255, BLUE);
	Figure *fc = new FilledCircle(300, 150, 25, RED, YELLOW);
	FilledEllipse *fe = new FilledEllipse(150, 300, 25, 100, RED, MAGENTA);
	Figure *fr = new FilledRectangle(400, 400, 500, 470, RED, CYAN);

	List *lst = new List();
	Colors *col = new Colors(30, 300, RED, CYAN);
	ActionButton *ab1 = new ActionButton("New", 100, 100, 150, 115);
	ActionButton *ab2 = new ActionButton("Save", 200, 100, 250, 115);
	ToolButton *tb1 = new ToolButton(t1, 100, 300, 0, 0);
	ToolButton *tb2 = new ToolButton(t2, 130, 300, 0, 0);
	ColorButton *cb1 = new ColorButton(BLUE, 11, 10, 50, 30);
	ColorButton *cb2 = new ColorButton(RED, 51, 10, 90, 30);

	free(a);

	lst->add(new Node(l)); lst->add(new Node(pl)); lst->add(new Node(c));
	lst->add(new Node(e)); lst->add(new Node(r)); lst->add(new Node(fc));
	lst->add(new Node(fe)); lst->add(new Node(fr));

	lst->drawAll();
	col->draw();
	ab1->draw();
	ab2->draw();
	tb1->draw();
	tb2->draw();
	cb1->draw();
	cb2->draw();

	int button;
	POINT pos;

	mouse_on();
	mouse_free(getmaxx()-1, getmaxy()-1);

	do
	{
		mouse_wait_for_press(2, &pos);
		Node* p = lst->changeStatus(pos.x, pos.y);
		mouse_off();
		if (p != NULL)
			p->getData()->draw();
		mouse_wait_for_release(2, &pos);
		mouse_on();
	}
	while (pos.x != 0);


	do
	{
		button = mouse_status(&pos);
		if (ab1->contains(pos.x, pos.y))
			ab1->handleMouse();
		if (ab2->contains(pos.x, pos.y))
			ab2->handleMouse();
	}
	while (button != 2);

	mouse_off();

	cleardevice();
	lst->moveAllSelected(100, 0);
	lst->removeAllSelected();
	lst->drawAll();

	getch();
	closegraph();

	lst->writeAll(stdout);
	delete lst;
	delete ab1;
	delete ab2;
	delete tb1;
	delete tb2;
	delete cb1;
	delete cb2;
*/
	ScreenManager *sm = new ScreenManager();
	sm->initializeComponents();
	sm->handleMouse();
	closegraph();
}