/*
 Programul trebuie sa extraga toate subsirurile crescatoare de lungime
maxima dintr-un sir dat.
*/

#include <stdio.h>
#include <conio.h>
#include <iostream.h>

void main()
{
	int n, s[100];

	clrscr();
	cout << "\n Numarul de elemente din sir: ";
	cin >> n;
	for (int i = 0; i < n; i++)
	{
		cout << "Elementul " << i << ": ";
		cin >> s[i];
	}



	return;
}
