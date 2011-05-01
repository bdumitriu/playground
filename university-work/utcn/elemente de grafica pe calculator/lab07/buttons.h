/*
 * Header file for on-screen buttons library.
 *
 * Author: Bogdan DUMITRIU
 * Date:   02.12.2001
 */

#include <conio.h>
#include <graphics.h>
#include <string.h>
#include "list.h"
#include "figures.h"
#include "mouse.h"

#ifndef __BUTTONS_H_
#define __BUTTONS_H_

/*
 * This class provides a means of implementing commands.
 * All button objects expect a Command object for the left
 * mouse button click and a Command object for the right
 * mouse button click. Such an object should be an insatnce
 * of a class that inherits from this Command class and
 * implements the doCommand method. This method is called
 * by all Button objects when they have to handle mouse clicks.
 */
class Command
{
public:
	Command()
	{}
	~Command()
	{}

	virtual void doCommand()
	{}
};

/*
 * This class simply stores the two colors used for drawing and
 * filling.
 */
class Colors
{
public:
	/*
	 * Builds a new Colors object with the specified forground
	 * and background color. x & y represent the coordinates
	 * of the upper left corner of the on-screen drawing
	 * of the two colors. The Colors representation will have
	 * a fixed size of 30 on the Ox axis and 10 on the Oy axis.
	 */
	Colors(int x, int y, int fgColor = LIGHTGRAY, int bgColor = BLACK)
	: x(x), y(y), fgColor(fgColor), bgColor(bgColor)
	{}
	~Colors()
	{}

	/*
	 * Sets the foreground color to fgColor.
	 */
	void setFgColor(int fgColor)
	{
		this->fgColor = fgColor;
	}
	/*
	 * Returns the current foreground color.
	 */
	int getFgColor()
	{
		return fgColor;
	}
	/*
	 * Sets the background color to bgColor.
	 */
	void setBgColor(int bgColor)
	{
		this->bgColor = bgColor;
	}
	/*
	 * Returns the current background color.
	 */
	int getBgColor()
	{
		return bgColor;
	}
	/*
	 * Draws the two colors on the screen at coordinates (x,y).
	 */
	void draw();
private:
	int bgColor;
	int fgColor;
	int x, y;
};

/*
 * This is a base class for all button objects. This
 * class should not be instantiated directly as its
 * functionality is limited. Its subclasses should be
 * instantiated instead.
 */
class Button
{
public:
	/*
	 * Builds a new Button with its upper right corner in (x1,y1)
	 * and its lower left corner in (x2,y2), with the specified
	 * border color. This button will have the leftClickCommand
	 * and rightClickCommand associated with it and will call
	 * their doCommand method when its leftClicked and rightClicked
	 * methods respectively are called.
	 */
	Button(int x1, int y1, int x2, int y2, int borderColor = WHITE,
		Command *leftClickCommand = NULL, Command *rightClickCommand = NULL)
	: x1(x1), y1(y1), x2(x2), y2(y2), color(borderColor), selected(0),
		leftCom(leftClickCommand), rightCom(rightClickCommand)
	{}
	~Button()
	{}

	/*
	 * Returns 1 if point (x,y) is contained in this button
	 * and 0 otherwise.
	 */
	virtual int contains(int x, int y);
	/*
	 * Draws the button on the screen.
	 */
	virtual void draw()
	{}
	/*
	 * Changes the status of the the button by selecting it
	 * if it is not selected and deselecting it if it is selected.
	 */
	virtual void changeStatus();
	/*
	 * Selects the button.
	 */
	virtual void select()
	{
		selected = 1;
	}
	/*
	 * Deselects the button.
	 */
	virtual void deselect()
	{
		selected = 0;
	}
	/*
	 * Gets the button's current status.
	 */
	virtual int getSelected()
	{
		return selected;
	}
	/*
	 * Calls leftClickCommand.doCommand().
	 */
	virtual void leftClicked();
	/*
	 * Calls rightClickCommand.doCommand().
	 */
	virtual void rightClicked();
	/* Handles the mouse while it is within the button area.
	 * It should be called only when the button enters the area.
	 * However, if it is called when the mouse is outside the
	 * area it will simply return without doing nothig. The method
	 * returns when the mouse is clicked or it exits the button area.
	 * It returns 0, 1, 2 if it has returned because the mouse left
	 * the area, the left button was clicked or the right button
	 * was clicked.
	 */
	virtual int handleMouse();
protected:
	int x1, y1;
	int x2, y2;
	int color;
	int selected;
	Command *leftCom;
	Command *rightCom;
};

/*
 * This class handles buttons with simple text inside that
 * select/deselect themselves automatically if the mouse is
 * over them and their handleMouse method is called.
 */
class ActionButton: public Button
{
public:
	/*
	 * Builds a new ActionButton with the specified text, with
	 * the upper right corner in (x1,y1) and the lower left corner
	 * in (x2,y2). The parameters leftClickCommand and
	 * rightClickCommand represent the objects whose doCommand
	 * method is called when the leftClicked and rightClicked
	 * methods of this object respectively are called.
	 */
	ActionButton(char text[20], int x1, int y1, int x2, int y2,
		int borderColor = WHITE, Command *leftClickCommand = NULL,
		Command *rightClickCommand = NULL, int normalDrawColor = RED,
		int selectedDrawColor = YELLOW, int normalBackgroundColor = BLACK,
		int selectedBackgroundColor = RED);
	~ActionButton()
	{};

	/*
	 * Draws the action button on the screen.
	 */
	void draw();
	/*
	 * Handles the mouse while it is within the button area.
	 * It should be called only when the button enters the area.
	 * However, if it is called when the mouse is outside the
	 * area it will simply return without doing nothig. The method
	 * returns when the mouse exits the button area. It always
	 * returns 0.
	 */
	int handleMouse();
private:
	char text[20];
	int col, bgCol;
	int selCol, selBgCol;
	int x, y;
};

/*
 * This class handles buttons that represent a tool. They
 * draw an image inside the button. The image is provided
 * via a 10x10 matrix of 0s and 1s. The pixel will be lit
 * if its corresponding matrix place is 1 and will not be
 * lit if it is 0.
 */
class ToolButton: public Button
{
public:
	/*
	 * Builds a new ToolButton with the specified image, with
	 * the upper right corner in (x1,y1) and the lower left corner
	 * in (x1+11,y1+11). The parameters leftClickCommand and
	 * rightClickCommand represent the objects whose doCommand
	 * method is called when the leftClicked and rightClicked
	 * methods of this object respectively are called.
	 */
	ToolButton(int pixelMatrix[10][10], int x1, int y1,
		int borderColor = WHITE, Command *leftClickCommand = NULL,
		Command *rightClickCommand = NULL, int normalTextColor = RED,
		int selectedTextColor = YELLOW, int normalBackgroundColor = BLACK,
		int selectedBackgroundColor = RED);
	~ToolButton()
	{}

	/*
	 * Draws the tool button on the screen.
	 */
	void draw();
private:
	int imgMatrix[10][10];
	int col, bgCol;
	int selCol, selBgCol;
};

/*
 * This class handles buttons that represent a single color.
 */
class ColorButton: public Button
{
public:
	/*
	 * Builds a new ColorButton with the specified color, with
	 * the upper right corner in (x1,y1) and the lower left corner
	 * in (x2,y2). The parameters leftClickCommand and
	 * rightClickCommand represent the objects whose doCommand
	 * method is called when the leftClicked and rightClicked
	 * methods of this object respectively are called.
	 */
	ColorButton(int color, int x1, int y1, int x2, int y2,
		Command *leftClickCommand = NULL, Command *rightClickCommand = NULL)
	: Button(x1, y1, x2, y2, color, leftClickCommand, rightClickCommand)
	{}
	~ColorButton()
	{}

	/*
	 * Draws the color button on the screen.
	 */
	void draw();
};

#endif