#include "c:\bc31\work\surse\new\clase\editor\editor.h"

Editor::Editor(int ExitCodes[30], int UpperLeftX, int UpperLeftY, int LowerRightX,
					int LowerRightY, int ForegroundColor, int BackgroundColor,
					char Title[20], int CleaningColor)
	: x1(UpperLeftX), y1(UpperLeftY), x2(LowerRightX), y2(LowerRightY),
	  fgc(ForegroundColor), bgc(BackgroundColor), cc(CleaningColor)
{
	for (int i = 0; i < 30; i++)
		if (ExitCodes[i] != 0)
			excod[i] = ExitCodes[i];
		else
		{
			dim = i;
			i = 30;
		}
	x_max = x2-x1-1;
	y_max = y2-y1-1;
	cur_x = 1;
	cur_y = 1;
	bi = 0;
	bj = 0;
	lines = 0;
	memcpy((char*) title, (char*) Title, 20);
	buffer = (char**) new char [50*50];
	for (i = 0; i < 50; i++)
		//for (int j = 0; j < 50; j++)
			strset(buffer[i], 32);
	memcpy((char*) title, (char*) Title, 20);
}

Editor::~Editor()
{
//	for (int i = 0; i < 500; i++)
		delete [] buffer;
}

void Editor::Initialize()
{
	textbackground(bgc);
	textcolor(WHITE);
	gotoxy(x1, y1);
	printf("É");
	for (int i = 0; i < x2-x1-1; i++)
		cprintf("Í");
	cprintf("»");
	for (i = y1+1; i < y2; i++)
	{
		gotoxy(x1, i);
		cprintf("º");
		gotoxy(x2, i);
		cprintf("º");
	}
	gotoxy(x1, y2);
	cprintf("È");
	for (i = 0; i < x2-x1-1; i++)
		cprintf("Í");
	cprintf("¼");
	(x2-x1-strlen(title))%2 == 0 ? gotoxy((x2-x1-strlen(title))/2, y1) : gotoxy((x2-x1-strlen(title)+1)/2, y1);
	cprintf(" %s ", title);
	window(x1+1, y1+1, x2-1, y2-1);
	clrscr();
	window(1, 1, 80, 25);
}

int Editor::NotExitCode(char c, int zero)
{
	int code;

	code = c;
	if (zero)
		code = -c;
	for (int i = 0; i < dim; i++)
		if (excod[i] == code)
			return 0;

	return 1;
}

int Editor::Edit()
{
	int ts;
	char c;

	window(x1+1, y1+1, x2-1, y2-1);
	gotoxy(cur_x, cur_y);
	textcolor(fgc);

	c = getch();
	while (NotExitCode(c, ts))
	{
		if (c == 0)
		{
			ts = 1;
			c = getch();
			if (c == 77)
			{
				cur_x++;
				bj++;
				if (cur_x == x_max+1)
				{
					cur_x--;
					for (int i = bi+1-cur_y; i < bi-cur_y+y_max; i++)
						for (int j = bj+1-cur_x; j < bj+1; j++)
							cprintf("%c", buffer[i][j]);
				}
				else
					gotoxy(cur_x, cur_y);
			}
		}
		else
		{
			ts = 0;
			if ((c > 31) && (c < 127))
			{
				cur_x++;
				buffer[bi][bj++] = c;
				if (cur_x == x_max+1)
				{
					cur_x--;
					for (int i = bi+1-cur_y; i < bi-cur_y+y_max; i++)
						for (int j = bj-x_max+2; j < bj+1; j++)
							cprintf("%c", buffer[i][j]);
					gotoxy(cur_x, cur_y);
				}
				else
					cprintf("%c", c);
			}
		}
		c = getch();
	}

}