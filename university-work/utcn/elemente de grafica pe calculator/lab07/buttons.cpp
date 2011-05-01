/*
 * Implementation for on-screen buttons library.
 *
 * Author: Bogdan DUMITRIU
 * Date:   02.12.2001
 */

#include "buttons.h"

void Colors::draw()
{
	setcolor(LIGHTGRAY);
	setfillstyle(SOLID_FILL, fgColor);
	bar3d(x, y, x+15, y+10, 0, 0);
	setfillstyle(SOLID_FILL, bgColor);
	bar3d(x+15, y, x+30, y+10, 0, 0);
}

int Button::contains(int x, int y)
{
	return ((x >= x1) && (x <= x2) && (y >= y1) && (y <= y2));
}

void Button::changeStatus()
{
	selected = !selected;
}

void Button::leftClicked()
{
	if (leftCom != NULL)
		leftCom->doCommand();
}

void Button::rightClicked()
{
	if (rightCom != NULL)
		rightCom->doCommand();
}

int Button::handleMouse()
{
	POINT pos;
	int button;

	button = mouse_status(&pos);
	if (!contains(pos.x, pos.y))
		return 0;
	while (contains(pos.x, pos.y))
	{
		if (button == 1)
		{
			mouse_wait_for_release(1, &pos);
			select();
			mouse_off();
			draw();
			mouse_on();
			leftClicked();
			return 1;
		}
		else
			if (button == 2)
			{
				mouse_wait_for_release(2, &pos);
				rightClicked();
				return 2;
			}
		button = mouse_status(&pos);
	}
	return 0;
}

ActionButton::ActionButton(char text[20], int x1, int y1, int x2, int y2,
	int borderColor, Command *leftClickCommand, Command *rightClickCommand,
	int normalTextColor, int selectedTextColor,
	int normalBackgroundColor, int selectedBackgroundColor)
: Button(x1, y1, x2, y2, borderColor, leftClickCommand, rightClickCommand),
	col(normalTextColor), bgCol(normalBackgroundColor),
	selCol(selectedTextColor), selBgCol(selectedBackgroundColor)
{
	strcpy(this->text, text);
	x = x1+(x2-x1-textwidth(text))/2;
	y = y1+(y2-y1-textheight(text))/2;
}

void ActionButton::draw()
{
	setcolor(color);
	rectangle(x1, y1, x2, y2);
	if (selected)
	{
		setcolor(selCol);
		setfillstyle(SOLID_FILL, selBgCol);
	}
	else
	{
		setcolor(col);
		setfillstyle(SOLID_FILL, bgCol);
	}
	bar(x1+1, y1+1, x2-1, y2-1);
	outtextxy(x, y, text);
}

int ActionButton::handleMouse()
{
	POINT pos;
	int button;

	button = mouse_status(&pos);
	if (!contains(pos.x, pos.y))
		return 0;

	select();
	mouse_off();
	draw();
	mouse_on();
	while (contains(pos.x, pos.y))
	{
		if (button == 1)
		{
			mouse_wait_for_release(1, &pos);
			leftClicked();
		}
		else
			if (button == 2)
			{
				mouse_wait_for_release(2, &pos);
				rightClicked();
			}
		button = mouse_status(&pos);
	}
	deselect();
	mouse_off();
	draw();
	mouse_on();
	return 0;
}

ToolButton::ToolButton(int pixelMatrix[10][10],
	int x1, int y1,
	int borderColor, Command *leftClickCommand, Command *rightClickCommand,
	int normalDrawColor, int selectedDrawColor,
	int normalBackgroundColor, int selectedBackgroundColor)
: Button(x1, y1, x1+11, y1+11, borderColor, leftClickCommand,
	rightClickCommand), col(normalDrawColor), bgCol(normalBackgroundColor),
	selCol(selectedDrawColor), selBgCol(selectedBackgroundColor)
{
	for (int i = 0; i < 10; i++)
		for (int j = 0; j < 10; j++)
			imgMatrix[i][j] = pixelMatrix[i][j];
}

void ToolButton::draw()
{
	int c;
	setcolor(color);
	rectangle(x1, y1, x2, y2);
	if (selected)
	{
		c = selCol;
		setfillstyle(SOLID_FILL, selBgCol);
	}
	else
	{
		c = col;
		setfillstyle(SOLID_FILL, bgCol);
	}

	bar(x1+1, y1+1, x2-1, y2-1);
	for (int i = 0; i < 10; i++)
		for (int j = 0; j < 10; j++)
			if (imgMatrix[i][j] == 1)
				putpixel(x1+j+1, y1+i+1, c);
}

void ColorButton::draw()
{
	setfillstyle(SOLID_FILL, color);
	bar(x1, y1, x2, y2);
}