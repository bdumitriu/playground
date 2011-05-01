#include "c:\bc31\work\surse\new\clase\editor\editor.h"

void main()
{
	int exc[30];
	exc[0] = -33;
	exc[1] = 27;
	exc[2] = 0;
	Editor ed(exc, 2, 2, 71, 20, RED, BLUE, "TEST scReeN", BLACK);

	textbackground(BLACK);
	clrscr();
	ed.Initialize();
	ed.Edit();

	getch();

}
