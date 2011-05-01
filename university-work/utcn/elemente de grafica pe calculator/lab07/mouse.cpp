#include <dos.h>
#include "mouse.h"

int mouse_reset(int *nr_of_buttons)
{
	union REGS inregs, outregs;

	inregs.x.ax = 0;				// functia Dos nr. 0
	int86(0x33, &inregs, &outregs);	// genereaza intreruperea soft INT 33h
	*nr_of_buttons = outregs.x.bx;	// nr. de butoane se afla in bx

	return outregs.x.ax;
}

void mouse_on()
{
	union REGS regs;

	regs.x.ax = 1;					// functia Dos nr. 1 - afisare cursor
	int86(0x33, &regs, &regs);
}

void mouse_off()
{
	union REGS regs;

	regs.x.ax = 2;					// functia Dos nr. 2 - ascundere cursor
	int86(0x33, &regs, &regs);
}

int mouse_status(POINT *position)
{
	union REGS regs;

	regs.x.ax = 3;		// functia Dos nr. 3 - intoarce starea butoanelor
						// si pozitia cursorului
	int86(0x33, &regs, &regs);
	position->x = regs.x.cx;		// registrul cx contine coordonata x
	position->y = regs.x.dx;		// registrul dx contine coordonata y
	return regs.x.bx;				// registrul bx contine starea butoanelor
}

void mouse_wait_for_press(int button, POINT *position)
{
	while (!(mouse_status(position) & button));
}

void mouse_wait_for_release(int button, POINT *position)
{
	while (mouse_status(position) & button);
}

void mouse_set_cursor(int picture[16][2])
{
	struct SREGS segregs;
	union REGS regs;

	segread(&segregs);
	regs.x.ax = 9;	// functia Dos nr. 9 - forma cursorului grafic mouse
	regs.x.bx = 0;	// abscisa punctului de referinta a cursorului in bx
	regs.x.cx = 0;	// ordonata punctului de referinta a cursorului in cx
	regs.x.dx = (int) picture;
	segregs.es = segregs.ds;
	int86x(0x33, &regs, &regs, &segregs);
}

void mouse_set_horizontal_pos(int minpos, int maxpos)
{
	union REGS regs;

	regs.x.ax = 7;		// functia Dos nr. 7 - coordonata minima si maxima
						// pe orizontala
	regs.x.cx = minpos;	// coordonata minima in reg. cx
	regs.x.dx = maxpos;	// coordonata maxima in reg. dx
	int86(0x33, &regs, &regs);
}

void mouse_set_vertical_pos(int minpos, int maxpos)
{
	union REGS regs;

	regs.x.ax = 8;		// functia Dos nr. 8 - coordonata minima si maxima
						// pe verticala
	regs.x.cx = minpos;	// coordonata minima in reg. cx
	regs.x.dx = maxpos;	// coordonata maxima in reg. dx
	int86(0x33, &regs, &regs);
}

void mouse_restrict(int minx, int miny, int maxx, int maxy)
{
	mouse_set_horizontal_pos(minx, maxx);
	mouse_set_vertical_pos(miny, maxy);
}

void mouse_free(int maxx, int maxy)
{
	mouse_set_horizontal_pos(0, maxx);
	mouse_set_vertical_pos(0, maxy);
}

void mouse_set_position(POINT *position)
{
	union REGS regs;

	regs.x.ax = 4;				// functia Dos nr. 4 - pozitionarea
								// cursorului de mouse
	regs.x.cx = position->x;	// coordonata orizontala in cx
	regs.x.dx = position->y;	// coordonata verticala in dx
	int86(0x33, &regs, &regs);
}