#include <stdio.h>
#include <conio.h>
#include <iostream.h>

int Putere(int a[10][10], int b[10][10], int m, int c[10][10])
{
	int i, j, k;

	for (i = 0; i < m; i++)
		for (j = 0; j < m; j++)
		{
			c[i][j] = 0;
			for (k = 0; k < m; k++)
				c[i][j] += a[i][k]*b[k][j];
		}

	return 0;
}

void main()
{
	int a[10][10], b[10][10], c[10][10], m, n, i, j, k;

	clrscr();
	cout << "\n";

	cout << " Nr. de linii (implicit si de coloane): ";
	cin >> m;

	cout << " Introduceti matricea: \n";
	for (i = 0; i < m; i++)
		for (j = 0; j < m; j++)
		{
			cout << "  elem[" << i+1 << "," << j+1 << "] = ";
			cin >> a[i][j];
			b[i][j] = a[i][j];
		}
	cout << "Puterea pana la care doriti sa vedeti: ";
	cin >> n;

	for (k = 1; k < n; k++)
	{
		Putere(a, b, m, c);
		clrscr();
		cout << "\n Matricea la puterea a " << k+1 << "-a: \n";
		for (i = 0; i < m; i++)
		{
			for (j = 0; j < m; j++)
			{
				cout << " " << c[i][j];
				a[i][j] = c[i][j];
			}
			cout << "\n";
		}
		getch();
	}

	return;
}