#include <stdio.h>
#include <iostream.h>
#include <conio.h>
#include <graphics.h>

enum pv {nu_e = 0, e = 1};
typedef pv **matrice;

void Creare(matrice m)
{
	//	setcolor
	for (int i = 1; i < 101; i++)
		for (int j = 1; j < 151; j++)
		{
			m[i][j] = 0;
			putpixel(i,j, GREEN);
		}

}

void main()
{
	int gdriver = DETECT, gmode;
	matrice mt;

	initgraph(&gdriver, &gmode, "c:\\bc31\\bgi");
	Creare(mt);
	getch();
	closegraph();
	int idx = 0;
	for (int i = 1; i < 101; i++)
		for (int j = 1; j < 151; j++)
			if (mt[i][j] == 1)
				idx++;
	cout << idx;
}