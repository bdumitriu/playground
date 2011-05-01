/*
 * Header file for the ScreenManager class.
 *
 * Author: Bogdan DUMITRIU
 * Date:   02.12.2001
 */

#include <graphics.h>
#include <stdio.h>
#include <conio.h>
#include "mouse.h"
#include "buttons.h"
#include "workarea.h"
#include "commands.h"

#ifndef __SCREENMANAGER_H_
#define __SCREENMANAGER_H_

/*
 * The number of buttons of each type.
 */
#define NR_OF_ACTION_BUTTONS 4
#define NR_OF_TOOL_BUTTONS 10
#define NR_OF_COLOR_BUTTONS 16

/*
 * This class handles an entire screen for a paint
 * program.
 */
class ScreenManager
{
public:
	/*
	 * Builds a new screen manager.
	 */
	ScreenManager()
	: action(1)
	{}
	~ScreenManager()
	{}

	void initializeComponents();
	void handleMouse();
private:
	ActionButton *ab[NR_OF_ACTION_BUTTONS];
	ToolButton *tb[NR_OF_TOOL_BUTTONS];
	ColorButton *cb[NR_OF_COLOR_BUTTONS];
	Colors *col;
	WorkArea *wa;
	int action;

    Button* contains(int x, int y);
};

#endif