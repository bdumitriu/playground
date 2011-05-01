/*
 * Header file for various classes that implement the Command class.
 *
 * Author: Bogdan DUMITRIU
 * Date:   02.12.2001
 */

#include <stdlib.h>
#include <dos.h>
#include "buttons.h"
#include "workarea.h"

#ifndef __COMMANDS_H_
#define __COMMANDS_H_

/*
 * Command associated with the "New" ActionButton.
 * It clears the workarea.
 */
class NewCommand: public Command
{
public:
	NewCommand(WorkArea *wa)
	: wa(wa)
	{}
	~NewCommand()
	{}

	void doCommand();
private:
	WorkArea *wa;
};

/*
 * Command associated with the "Save" ActionButton.
 */
class SaveCommand: public Command
{
public:
	SaveCommand(WorkArea *wa)
	: wa(wa)
	{}
	~SaveCommand()
	{}

	void doCommand();
private:
	WorkArea *wa;
};

/*
 * Command associated with the "Load" ActionButton.
 */
class LoadCommand: public Command
{
public:
	LoadCommand(WorkArea *wa)
	: wa(wa)
	{}
	~LoadCommand()
	{}

	void doCommand();
private:
	WorkArea *wa;
};

/*
 * Command associated with the "Exit" ActionButton.
 */
class ExitCommand: public Command
{
public:
	ExitCommand(WorkArea *wa)
	: wa(wa)
	{}
	~ExitCommand()
	{}

	void doCommand();
private:
	WorkArea *wa;
};

/*
 * Command class associated with the ToolButtons.
 */
class ToolCommand: public Command
{
public:
	ToolCommand(WorkArea *wa, int action)
	: wa(wa), action(action)
	{}
	~ToolCommand()
	{}

	void doCommand();
private:
	WorkArea *wa;
	int action;
};

/*
 * Command class associated with the ColorButtons for left mouse click.
 */
class ColorCommandLeft: public Command
{
public:
	ColorCommandLeft(Colors *col, int color)
	: col(col), color(color)
	{}
	~ColorCommandLeft()
	{}

	void doCommand();
private:
	Colors *col;
	int color;
};

/*
 * Command class associated with the ColorButtons for right mouse click.
 */
class ColorCommandRight: public Command
{
public:
	ColorCommandRight(Colors *col, int color)
	: col(col), color(color)
	{}
	~ColorCommandRight()
	{}

	void doCommand();
private:
	Colors *col;
	int color;
};

#endif