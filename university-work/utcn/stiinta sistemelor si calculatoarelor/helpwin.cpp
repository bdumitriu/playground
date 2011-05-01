#include "helpwin.h"

/*
 * Autor:		Bogdan DUMITRIU
 * Data:		07.05.2002
 * Versiune:	0.1
 *
 * Implementarea claselor HelpWin si TInterior.
 */

/*
 * Construieste un nou obiect de tip TInterior.
 */
TInterior::TInterior(const TRect& bounds, TScrollBar *aHScrollBar,
	TScrollBar *aVScrollBar)
: TScroller(bounds, aHScrollBar, aVScrollBar), lineCount(0)
{
	options = options | ofFramed;
}

/*
 * Sterge obiectul de tip TInterior.
 */
void TInterior::~TInterior()
{
	for (int i = 0; i < lineCount; i++)
		delete lines[i];
}

/*
 * Incarca continutul fisierului in buffere interne.
 */
void TInterior::readFile(const char *fileName)
{
	ifstream fileToView(fileName);
	if (fileToView)
	{
		char buf[maxLineLength];
		while (lineCount < maxLines &&
			fileToView.getline(buf, maxLineLength) != 0)
		{
			lines[lineCount] = newStr(buf);
			lineCount++;
		}
	}
}

/*
 * Deseneaza textul pe ecran.
 */
void TInterior::draw()
{
	setLimit(maxLineLength, lineCount);
	ushort color = getColor(0x0301);
	for (int i = 0; i < size.y; i++)
	{
		TDrawBuffer b;
		b.moveChar(0, ' ', color, size.x);	// umplu cu spatii

		int j = delta.y + i;

		if (j < lineCount && lines[j] != 0)
		{
			char s[maxLineLength];
			if (delta.x > strlen(lines[j]))
				s[0] = EOS;
			else
			{
				strncpy(s, lines[j] + delta.x, size.x);
				s[size.x] = EOS;
			}

			b.moveCStr(0, s, color);
		}

		writeLine(0, i, size.x, 1, b);
	}
}

/*
 * Construieste un nou obiect de tip HelpWin care va contine
 * textul din fisierul primit ca parametru.
 */
HelpWin::HelpWin(const TRect& bounds, const char *aTitle,
	short aNumber, char *fileName)
: TWindow(bounds, aTitle, aNumber), TWindowInit(&HelpWin::initFrame)
{
	this->fileName = fileName;
	TRect bnds = getExtent();
	TRect r(bnds.a.x, bnds.a.y, bnds.b.x, bnds.b.y);
	interior = makeInterior(r);
	if (interior != NULL)
	{
		interior->readFile(fileName);
		interior->growMode = gfGrowHiX | gfGrowHiY;
		insert(interior);
	}
}

/*
 * Creeaza un nou TInterior cu coordonatele primite ca parametru.
 */
TInterior *HelpWin::makeInterior(const TRect& bounds)
{
	TRect r = TRect(bounds.b.x-1, bounds.a.y+1, bounds.b.x, bounds.b.y-1);
	TScrollBar *vScrollBar = new TScrollBar(r);
	if (vScrollBar != 0)
	{
		vScrollBar->options |= ofPostProcess;
		insert(vScrollBar);
		r = TRect(bounds.a.x+2, bounds.b.y-1, bounds.b.x-2, bounds.b.y);
		TScrollBar *hScrollBar = new TScrollBar(r);
		if (hScrollBar != 0)
		{
			hScrollBar->options |= ofPostProcess;
			insert(hScrollBar);
			r = bounds;
			r.grow(-1, -1);
			return new TInterior(r, hScrollBar, vScrollBar);
		}
		else
			return NULL;
	}
	else
		return NULL;
}