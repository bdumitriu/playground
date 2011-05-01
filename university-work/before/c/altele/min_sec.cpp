// Program pentru adunat minute&secunde
#include <stdio.h>
#include <conio.h>
#include <iostream.h>

void main()
{
	int n, m[100], s[100];

	clrscr();
	cout << "\n Numarul de timpi: ";
	cin >> n;
	for (int i = 0; i < n; i++)
	{
		cout << " Timpul " << i+1 << ".minute = ";
		cin >> m[i];
		cout << " Timpul " << i+1 << ".secunde = ";
		cin >> s[i];
	}

	int suma = 0;
	for (i = 0; i < n; i++)
	{
		suma += s[i]+m[i]*60;
	}
	cout << "\n TIMP TOTAL: " << int(suma/60) << " MINUTE SI " << suma%60
		  << " SECUNDE.";
	getch();

	return;
}