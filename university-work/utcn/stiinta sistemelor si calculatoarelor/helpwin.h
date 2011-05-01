/*
 * Autor:		Bogdan DUMITRIU
 * Data:		07.05.2002
 * Versiune:	0.1
 *
 * Aceasta clasa defineste o fereastra care afiseaza intr-un cadru
 * TScroller continutul unui fisier. Clasa a fost construita in
 * mare parte pe baza unui exemplu care vine implicit cu biblioteca
 * Turbo Vision, folosita aici.
 */

#define Uses_TEventQueue
#define Uses_TEvent
#define Uses_TProgram
#define Uses_TApplication
#define Uses_TKeys
#define Uses_TRect
#define Uses_TView
#define Uses_TWindow
#define Uses_TScroller
#define Uses_TScrollBar
#define Uses_TSItem
#define Uses_TLabel

#include <stdlib.h>
#include <iostream.h>
#include <fstream.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <tv.h>

#ifndef __HELPWIN_H_
#define __HELPWIN_H_

const int cmMyNewWin   = 201;
const int cmNewDialog  = 202;
const int maxLineLength = maxViewWidth+1;
const int maxLines = 100;

class TInterior: public TScroller
{
public:
	TInterior(const TRect& bounds, TScrollBar *aHScrollBar,
		TScrollBar *aVScrollBar);
	~TInterior();

	void readFile(const char *fileName);
	virtual void draw();

private:
	int lineCount;
	char *lines[maxLines];
};

class HelpWin: public TWindow
{
public:
	HelpWin(const TRect& bounds, const char *aTitle, short aNumber,
		char *fileName);

	TInterior *makeInterior(const TRect& r);

private:
	TInterior *interior;
	char *fileName;
};

#endif