/*
 * Implementation for the ScreenManager class.
 *
 * Author: Bogdan DUMITRIU
 * Date:   02.12.2001
 */

#include "manager.h"

/*
 * The matrices that represent the signs on the tool buttons.
 */

// selection image
int m0[10][10] = {{0,0,0,0,0,0,0,0,0,0},
				  {0,0,1,1,0,0,1,1,0,0},
				  {0,1,0,0,0,0,0,0,1,0},
				  {0,1,0,0,0,0,0,0,1,0},
				  {0,0,0,0,0,0,0,0,0,0},
				  {0,0,0,0,0,0,0,0,0,0},
				  {0,1,0,0,0,0,0,0,1,0},
				  {0,1,0,0,0,0,0,0,1,0},
				  {0,0,1,1,0,0,1,1,0,0},
				  {0,0,0,0,0,0,0,0,0,0}};

// line image
int m1[10][10] = {{0,0,0,0,0,0,0,0,0,0},
				  {0,0,0,0,0,0,0,0,1,0},
				  {0,0,0,0,0,0,0,1,0,0},
				  {0,0,0,0,0,0,1,0,0,0},
				  {0,0,0,0,0,1,0,0,0,0},
				  {0,0,0,0,1,0,0,0,0,0},
				  {0,0,0,1,0,0,0,0,0,0},
				  {0,0,1,0,0,0,0,0,0,0},
				  {0,1,0,0,0,0,0,0,0,0},
				  {0,0,0,0,0,0,0,0,0,0}};

// polyline image
int m2[10][10] = {{0,0,0,0,0,0,0,0,0,0},
				  {0,1,1,1,1,1,1,1,1,0},
				  {0,0,0,0,0,0,0,1,0,0},
				  {0,0,0,0,1,0,1,0,0,0},
				  {0,0,0,1,0,1,0,0,0,0},
				  {0,0,1,0,0,1,0,0,0,0},
				  {0,1,0,0,0,1,1,1,1,0},
				  {0,0,1,0,0,0,0,0,1,0},
				  {0,0,0,1,1,1,1,1,1,0},
				  {0,0,0,0,0,0,0,0,0,0}};

// polygon image
int m3[10][10] = {{0,0,0,0,0,0,0,0,0,0},
				  {0,1,1,1,1,1,1,1,1,0},
				  {0,1,0,0,0,0,0,0,1,0},
				  {0,1,0,0,0,0,0,1,0,0},
				  {0,1,0,0,0,0,1,0,0,0},
				  {0,1,0,0,0,0,1,0,0,0},
				  {0,0,1,0,0,1,0,0,0,0},
				  {0,0,0,1,0,1,0,0,0,0},
				  {0,0,0,0,1,0,0,0,0,0},
				  {0,0,0,0,0,0,0,0,0,0}};

// circle image
int m4[10][10] = {{0,0,0,0,0,0,0,0,0,0},
				  {0,0,0,1,1,1,1,0,0,0},
				  {0,0,1,0,0,0,0,1,0,0},
				  {0,1,0,0,0,0,0,0,1,0},
				  {0,1,0,0,0,0,0,0,1,0},
				  {0,1,0,0,0,0,0,0,1,0},
				  {0,1,0,0,0,0,0,0,1,0},
				  {0,0,1,0,0,0,0,1,0,0},
				  {0,0,0,1,1,1,1,0,0,0},
				  {0,0,0,0,0,0,0,0,0,0}};

// filled circle image
int m5[10][10] = {{0,0,0,0,0,0,0,0,0,0},
				  {0,0,0,1,1,1,1,0,0,0},
				  {0,0,1,1,1,1,1,1,0,0},
				  {0,1,1,1,1,1,1,1,1,0},
				  {0,1,1,1,1,1,1,1,1,0},
				  {0,1,1,1,1,1,1,1,1,0},
				  {0,1,1,1,1,1,1,1,1,0},
				  {0,0,1,1,1,1,1,1,0,0},
				  {0,0,0,1,1,1,1,0,0,0},
				  {0,0,0,0,0,0,0,0,0,0}};

// ellipse image
int m6[10][10] = {{0,0,0,0,0,0,0,0,0,0},
				  {0,0,0,0,0,0,0,0,0,0},
				  {0,0,0,1,1,1,1,0,0,0},
				  {0,0,1,0,0,0,0,1,0,0},
				  {0,1,0,0,0,0,0,0,1,0},
				  {0,1,0,0,0,0,0,0,1,0},
				  {0,0,1,0,0,0,0,1,0,0},
				  {0,0,0,1,1,1,1,0,0,0},
				  {0,0,0,0,0,0,0,0,0,0},
				  {0,0,0,0,0,0,0,0,0,0}};

// filled ellipse image
int m7[10][10] = {{0,0,0,0,0,0,0,0,0,0},
				  {0,0,0,0,0,0,0,0,0,0},
				  {0,0,0,1,1,1,1,0,0,0},
				  {0,0,1,1,1,1,1,1,0,0},
				  {0,1,1,1,1,1,1,1,1,0},
				  {0,1,1,1,1,1,1,1,1,0},
				  {0,0,1,1,1,1,1,1,0,0},
				  {0,0,0,1,1,1,1,0,0,0},
				  {0,0,0,0,0,0,0,0,0,0},
				  {0,0,0,0,0,0,0,0,0,0}};

// rectangle image
int m8[10][10] = {{0,0,0,0,0,0,0,0,0,0},
				  {0,0,0,0,0,0,0,0,0,0},
				  {0,1,1,1,1,1,1,1,1,0},
				  {0,1,0,0,0,0,0,0,1,0},
				  {0,1,0,0,0,0,0,0,1,0},
				  {0,1,0,0,0,0,0,0,1,0},
				  {0,1,0,0,0,0,0,0,1,0},
				  {0,1,1,1,1,1,1,1,1,0},
				  {0,0,0,0,0,0,0,0,0,0},
				  {0,0,0,0,0,0,0,0,0,0}};

// filled rectangle image
int m9[10][10] = {{0,0,0,0,0,0,0,0,0,0},
				  {0,0,0,0,0,0,0,0,0,0},
				  {0,1,1,1,1,1,1,1,1,0},
				  {0,1,1,1,1,1,1,1,1,0},
				  {0,1,1,1,1,1,1,1,1,0},
				  {0,1,1,1,1,1,1,1,1,0},
				  {0,1,1,1,1,1,1,1,1,0},
				  {0,1,1,1,1,1,1,1,1,0},
				  {0,0,0,0,0,0,0,0,0,0},
				  {0,0,0,0,0,0,0,0,0,0}};

void ScreenManager::initializeComponents()
{
	// divide the screen in several different parts
	setcolor(LIGHTGRAY);
	line(0, 0, getmaxx(), 0);
	line(0, 0, 0, getmaxy());
	line(0, getmaxy(), getmaxx(), getmaxy());
	line(getmaxx(), 0, getmaxx(), getmaxy());
	line(0, 49, getmaxx(), 49);
	line(39, 0, 39, getmaxy());
	line(39, 451, getmaxx(), 451);
	line(0, 199, 39, 199);

	// write the PAINT in the upper left corner
	setcolor(RED);
	outtextxy(2, 5, "P");
	outtextxy(textwidth("P"), 5+textheight("P"), "A");
	outtextxy(textwidth("PA"), 5+2*textheight("P"), "I");
	outtextxy(textwidth("PAI"), 5+3*textheight("P"), "N");
	outtextxy(textwidth("PAIN"), 5+4*textheight("P"), "T");

	// create the forground/background color box
	col = new Colors(5, 205);

	// create the work area
	wa = new WorkArea(40, 50, getmaxx()-1, 450, BLACK, col);
	wa->setAction(1);

	// create the action buttons
	ab[0] = new ActionButton("New", 100, 10, 150, 25, WHITE,
		new NewCommand(wa));
	ab[1] = new ActionButton("Save", 200, 10, 250, 25, WHITE,
		new SaveCommand(wa));
	ab[2] = new ActionButton("Load", 300, 10, 350, 25, WHITE,
		new LoadCommand(wa));
	ab[3] = new ActionButton("Exit", 400, 10, 450, 25, WHITE,
		new ExitCommand(wa));

	// create the tool buttons
	tb[0] = new ToolButton(m0, 5, 60, WHITE, new ToolCommand(wa, 0));
	tb[1] = new ToolButton(m1, 23, 60, WHITE, new ToolCommand(wa, 1));
	tb[2] = new ToolButton(m2, 5, 90, WHITE, new ToolCommand(wa, 2));
	tb[3] = new ToolButton(m3, 23, 90, WHITE, new ToolCommand(wa, 3));
	tb[4] = new ToolButton(m4, 5, 120, WHITE, new ToolCommand(wa, 4));
	tb[5] = new ToolButton(m5, 23, 120, WHITE, new ToolCommand(wa, 5));
	tb[6] = new ToolButton(m6, 5, 150, WHITE, new ToolCommand(wa, 6));
	tb[7] = new ToolButton(m7, 23, 150, WHITE, new ToolCommand(wa, 7));
	tb[8] = new ToolButton(m8, 5, 180, WHITE, new ToolCommand(wa, 8));
	tb[9] = new ToolButton(m9, 23, 180, WHITE, new ToolCommand(wa, 9));
	tb[action]->select();

	// draw all components created so far on the screen
	col->draw();
	wa->draw();
	for (int i = 0; i < NR_OF_ACTION_BUTTONS; i++)
		ab[i]->draw();
	for (i = 0; i < NR_OF_TOOL_BUTTONS; i++)
		tb[i]->draw();

	// write F and B under the foreground/background color box
	setcolor(RED);
	outtextxy(6, 220, "F");
	outtextxy(28, 220, "B");

	// create & draw the color buttons
	for (i = 0; i < NR_OF_COLOR_BUTTONS; i++)
	{
		if (i%2 == 0)
			cb[i] = new ColorButton(i, 6, 251+(i/2)*15, 19, 249+(i/2+1)*15,
				new ColorCommandLeft(col, i), new ColorCommandRight(col, i));
		else
			cb[i] = new ColorButton(i, 21, 251+(i/2)*15, 34, 249+(i/2+1)*15,
				new ColorCommandLeft(col, i), new ColorCommandRight(col, i));
		cb[i]->draw();
		setcolor(LIGHTGRAY);
		if (i%2 == 0)
			rectangle(5, 250+(i/2)*15, 20, 250+(i/2+1)*15);
		else
			rectangle(20, 250+(i/2)*15, 35, 250+(i/2+1)*15);
	}

	mouse_free(getmaxx(), getmaxy());
}

Button* ScreenManager::contains(int x, int y)
{
	for (int i = 0; i < NR_OF_TOOL_BUTTONS; i++)
		if (tb[i]->contains(x, y))
			return tb[i];
	for (i = 0; i < NR_OF_ACTION_BUTTONS; i++)
		if (ab[i]->contains(x, y))
			return ab[i];
	for (i = 0; i < NR_OF_COLOR_BUTTONS; i++)
		if (cb[i]->contains(x, y))
			return cb[i];
	return NULL;
}

void ScreenManager::handleMouse()
{
	POINT pos;
	Button *button;

	mouse_on();
	do
	{
		mouse_status(&pos);
		button = contains(pos.x, pos.y);
		if (button != NULL)
		{
			if (button->handleMouse() == 1)
			{
				if (action != wa->getAction())
				{
					tb[action]->deselect();
					mouse_off();
					tb[action]->draw();
					mouse_on();
					action = wa->getAction();
					if (action != 0)
					{
						wa->deselect();
						wa->draw();
					}
				}
			}
		}
		else
		{
			if (wa->contains(pos.x, pos.y))
				wa->handleMouse();
		}
	}
	while (1);
}