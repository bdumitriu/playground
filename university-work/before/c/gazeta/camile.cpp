#include <stdio.h>
#include <conio.h>
#include <stdlib.h>
#include <string.h>
#include <fstream.h>
#include <iostream.h>

int x[31];
long nrsol = 0;

int VerificareDateIntrare(int NrCamile)
{
	if (NrCamile <= 0)
	{
		cout<<" Ar fi indicat sa introduceti un numar mai mare ca 0 de camile.";
		getch();
		return 0;
	}

	if (NrCamile == 1)
	{
		cout<<" O singura camila n-are pe cine sa se supere.";
		getch();
		return 0;
	}

	return 1;
}

void ProcesareSolutie(int n)
{
	for (int j = 1; j <= n; j++)
		cout<<x[j]<<" ";
	cout<<"\n";
}

int Ok(int i)
{
	if (x[i-1] == x[i]+1)
		return 0;
	for (int j = 1; j < i; j++)
		if (x[j] == x[i])
			return 0;
	return 1;
}

int CalculeazaNumarPosibilitati(int n)
{
	int i = 1, cont;

	x[i] = 0;
	do
	{
		while (x[i] < n)
		{
			x[i]++;
			if (Ok(i))
				if (i == n)
				{	ProcesareSolutie(n);
					nrsol++;}

				else
					x[++i] = 0;
		}
		i--;
	}
	while(i > 0);

	return 0;
}

void main()
{
	char *s = new char[20];
	int NrCamile;

	ifstream f("camile.in");
	ofstream g("camile.out");

	clrscr();
	printf("\n");

	f.getline(s, 10);
	f.close();
	NrCamile = atoi(s);
	NrCamile = 10;
	delete [] s;

	if (!(VerificareDateIntrare(NrCamile)))
		return;

	CalculeazaNumarPosibilitati(NrCamile);
	cout<<nrsol;
}