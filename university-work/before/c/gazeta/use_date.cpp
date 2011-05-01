#include "date.h"
#include <conio.h>

void main()
{
	Date d = "1998a2a8";
	int  d_err = 0;

	clrscr();
	int x;
	x = YMD(d, &d_err);
	printf(" %d ", x);
	printf("\n %d ", d_err);
	getch();
}