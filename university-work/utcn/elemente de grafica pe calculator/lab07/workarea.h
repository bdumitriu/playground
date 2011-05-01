/*
 * Header file for the workarea class.
 *
 * Author: Bogdan DUMITRIU
 * Date:   02.12.2001
 */

#include <graphics.h>
#include <stdio.h>
#include <conio.h>
#include "mouse.h"
#include "figures.h"
#include "buttons.h"
#include "list.h"

#ifndef __WORKAREA_H_
#define __WORKAREA_H_

/*
 * This class handles the area of the screen where the actual
 * drawing is done.
 */
class WorkArea
{
public:
	/*
	 * Builds a new WorkArea with the upper right corner in
	 * (x1,y1), the lower left corner in (x2,y2) and the specified
	 * background color. The colors parameter should be a valid
	 * (non NULL) Colors object as it is used for passing color
	 * values to the figures drawn within the workarea.
	 */
	WorkArea(int x1, int y1, int x2, int y2, int backgroundColor,
		Colors *colors);
	~WorkArea();

	/*
	 * Sets the current action for the workarea to action.
	 * Based on the value of action mouse clicks are handled
	 * as follows:
	 *  - action - what is done -
	 *    0        selection/movement/deletion of items is handled.
	 *    1        a line is drawn.
	 *    2        a polyline is drawn.
	 *    3        a polygon is drawn.
	 *    4        a circle is drawn.
	 *    5        a filled circle is drawn.
	 *    6        an ellipse is drawn.
	 *    7        a filled ellipse is drawn.
	 *    8        a rectangle is drawn.
	 *    9        a filled rectangle is drawn.
	 */
	void setAction(int action)
	{
		this->action = action;
	}
	/*
	 * Returns the current workarea action.
	 */
	int getAction()
	{
		return action;
	}
	/*
	 * Redraws the entire workarea.
	 */
	void draw();
	/*
	 * Deselects all objects.
	 */
	void deselect();
	/*
	 * Returns 1 if point (x,y) is inside the workarea and 0
	 * otherwise.
	 */
	int contains(int x, int y);
	/*
	 * Handles the mouse events that take place inside the workarea.
	 * It should be called when the mouse enters the workarea. The
	 * method returns when the mouse leaves the workarea. However,
	 * if the method is called with the mouse outside the workarea
	 * it returns immediately without doing nothing.
	 */
	void handleMouse();
	/*
	 * Saves all items to the specified file. The file should be
	 * open for write before the call to this method and closed
	 * after it.
	 */
	void save(FILE *f);
	/*
	 * Drops its current list of items and replaces it with what is
	 * found in the specified file. The file should be open for read
	 * before the call to this method and closed after it.
	 */
	void load(FILE *f);
	/*
	 * Causes the object to drop its current list of items and
	 * reinitialize it with a new list. The draw method should
	 * be called after this in order to clear the screen as well.
	 */
	void clear();
private:
	int x1, y1;
	int x2, y2;
	int bgCol;
	Colors *col;
	List *items;
	int action;
};

#endif