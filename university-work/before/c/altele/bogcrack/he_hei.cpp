#include <stdio.h>
#include <conio.h>
#include <string.h>
#include <dir.h>
#include <stdlib.h>
#include <iostream.h>
#include <fstream.h>

enum Boolean {True = 1, False = 0};
char tab[4][4];
int xx[8] = {-1, -1, 0, 1, 1, 1, 0, -1};
int yy[8] = {0, -1, -1, -1, 0, 1, 1, 1};

Boolean SCanBeObtainedFromTab(char s[20])
{
	int i = 0, x;
	char sol[16];
	char *aux1 = new char, *aux2 = new char;

	aux1 = &s[i];
	for (int j = 0; j < 4; j++)
		for (int k = 0; k < 4; k++)
		{
			aux2 = &tab[j][k];
			if (!(strcmp(aux1, aux2)))
			{
				x = 0;
				for (int y = 0; y < 9; y++)
					if ()
			}
		}
}

void main()
{
	char *s = new char [20];
	char solutie[200][20];
	int k = 0;

//	chdir("c:\\bc31\\work\\surse\\altele\\bogcrack\\");
	clrscr();

	cout<<"\n";
/*	for (int i = 0; i < 4; i++)
		for (int j = 0; j < 4; j++)
		{
			cout<<" Litera din pozitia "<<i+1<<", "<<j+1<<" este : ";
			cin>>tab[i][j];
		}*/

	ifstream f("he_hei.dat");
	while(!f.eof())
	{
		f.getline(s, 20);
		if (SCanBeObtainedFromTab(s))
			strcpy(solutie[++k], s);
	}

	getch();
}