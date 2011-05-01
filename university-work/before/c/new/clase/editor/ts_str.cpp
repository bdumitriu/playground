#include "str.h"
#include <conio.h>

void main()
{
	String s[10];

	clrscr();
	for (int i = 0; i < 100; i++)
	{
		s[i].SetDim();
		s[i].SetStringToChar(' ');
	}
	textbackground(BLUE);
	s[3].Print(1, 10);

	textbackground(BLACK);
}
