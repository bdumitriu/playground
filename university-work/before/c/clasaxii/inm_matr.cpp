#include <stdio.h>
#include <conio.h>
#include <iostream.h>

void main()
{
	int a[10][10], b[10][10], c[10][10], i, j, k, m, n, p;

	clrscr();
	cout << "\n";

	cout << " Numarul de linii ale primei matrice: ";
	cin >> m;
	cout << " Numarul de coloane ale primei matrice (implicit cel de linii ale celei de-a doua): ";
	cin >> n;
	cout << " Numarul de coloane ale celei de-a doua matrice: ";
	cin >> p;

	cout << " Introduceti prima matrice: \n";
	for (i = 0; i < m; i++)
		for (j = 0; j < n; j++)
		{
			cout << "  elem[" << i+1 << "," << j+1 << "] = ";
			cin >> a[i][j];
		}
	cout << " Si acum cea de-a doua: \n";
	for (i = 0; i < n; i++)
		for (j = 0; j < p; j++)
		{
			cout << "  elem[" << i+1 << "," << j+1 << "] = ";
			cin >> b[i][j];
		}

	for (i = 0; i < m; i++)
		for (j = 0; j < p; j++)
		{
			c[i][j] = 0;
			for (k = 0; k < n; k++)
				c[i][j] += a[i][k]*b[k][j];
		}

	clrscr();
	cout << "\n Matricea produs este: \n";
	for (i = 0; i < m; i++)
	{
		for (j = 0; j < p; j++)
			cout << " " << c[i][j];
		cout << "\n";
	}

	getch();
	return;
}