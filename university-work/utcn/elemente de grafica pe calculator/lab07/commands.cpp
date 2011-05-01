/*
 * Implementation for various classes that implement the Command class.
 *
 * Author: Bogdan DUMITRIU
 * Date:   02.12.2001
 */

#include "commands.h"

void NewCommand::doCommand()
{
	wa->clear();
}

void SaveCommand::doCommand()
{
	int idx = 0;
	char name[13];
	int xx = 45+textwidth("File (without extension): ");

	setcolor(RED);
	name[0] = '\0';
	outtextxy(45, 455, "File (without extension): ");
	char c;
	do
	{
		c = getch();
		if (c == 0)
			c = getch();
		int tst = 0;
		if ((c >= 97) && (c <= 122))
			tst = 1;
		if ((c >= 65) && (c <= 90))
			tst = 1;
		if ((c >= 48) && (c <= 57))
			tst = 1;
		if ((c == 45) || (c == 95) || (c == 126))
			tst = 1;
		if (tst == 1)
		{
			name[idx] = c;
			idx++;
			name[idx] = '\0';
			outtextxy(xx, 455, name);
		}
	}
	while ((idx < 8) && (c != 13));
	name[idx++] = '.';
	name[idx++] = 'p';
	name[idx++] = 'n';
	name[idx++] = 't';
	name[idx] = '\0';

	FILE *f;

	setcolor(BLACK);
	outtextxy(45, 455, "лллллллллллллллллллллллллллллллллллллллллллллллл");
	setcolor(RED);
	if ((f = fopen(name, "w")) == NULL)
		outtextxy(45, 455, "File could not be opened.");
	else
	{
		wa->save(f);
		fclose(f);
		outtextxy(45, 455, "File saved.");
	}
	delay(1000);
	setcolor(BLACK);
	outtextxy(45, 455, "лллллллллллллллллллллллллллллллллллллллллллллллл");
}

void LoadCommand::doCommand()
{
	int idx = 0;
	char name[13];
	int xx = 45+textwidth("File (without extension): ");

	setcolor(RED);
	name[0] = '\0';
	outtextxy(45, 455, "File (without extension): ");
	char c;
	do
	{
		c = getch();
		if (c == 0)
			c = getch();
		int tst = 0;
		if ((c >= 97) && (c <= 122))
			tst = 1;
		if ((c >= 65) && (c <= 90))
			tst = 1;
		if ((c >= 48) && (c <= 57))
			tst = 1;
		if ((c == 45) || (c == 95) || (c == 126))
			tst = 1;
		if (tst == 1)
		{
			name[idx] = c;
			idx++;
			name[idx] = '\0';
			outtextxy(xx, 455, name);
		}
	}
	while ((idx < 8) && (c != 13));
	name[idx++] = '.';
	name[idx++] = 'p';
	name[idx++] = 'n';
	name[idx++] = 't';
	name[idx] = '\0';

	FILE *f;

	setcolor(BLACK);
	outtextxy(45, 455, "лллллллллллллллллллллллллллллллллллллллллллллллл");
	setcolor(RED);
	if ((f = fopen(name, "r")) == NULL)
		outtextxy(45, 455, "File could not be opened.");
	else
	{
		wa->load(f);
		fclose(f);
		setcolor(RED);
		outtextxy(45, 455, "File loaded.");
	}
	delay(1000);
	setcolor(BLACK);
	outtextxy(45, 455, "лллллллллллллллллллллллллллллллллллллллллллллллл");
}

void ExitCommand::doCommand()
{
	wa->clear();
	closegraph();
    mouse_off();
	exit(0);
}

void ToolCommand::doCommand()
{
	wa->setAction(action);
}

void ColorCommandLeft::doCommand()
{
	col->setFgColor(color);
	col->draw();
}

void ColorCommandRight::doCommand()
{
	col->setBgColor(color);
	col->draw();
}