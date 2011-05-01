#include <stdio.h>
#include <dos.h>
#include <iostream.h>
#include <fstream.h>
#include <conio.h>
#include <string.h>
#include <stdlib.h>

int x[100], idx = 0;

int test(int n)
{
	for (int i = 0; i < idx; i++)
		if (x[i] == n)
			return 0;

	return 1;
}

void main()
{
	int nr_lectie, cv, cls;
	char filename[56], str[3];
	struct time t1, t2;

	clrscr();
	cout << "\n";

	// crearea variabilei cu numele fisierului in functie de lectie si de
	//
	do
	{
		cout << " CLASA: ";
		cin >> cls;
	}
	while ((cls < 5) || (cls > 6));
	do
	{
		cout << " LECTIA: ";
		cin >> nr_lectie;
	}
	while ((nr_lectie < 0) || (nr_lectie > 16));
	do
	{
		cout << " CUVINTE SAU VERBE (1/2): ";
		cin >> cv;
	}
	while ((cv != 1) && (cv != 2));
	filename[0] = '\0';
	strcat(filename, "c:\\bc31\\work\\surse\\altele\\italiana\\diction\\");
	switch(cls)
	{
		case 5:
		{
			strcat(filename, "5");
			break;
		}
		case 6:
		{
			strcat(filename, "6");
			break;
		}
	}
	if (!int(nr_lectie/10))
		strcat(filename, "0");
	str[0] = '\0';
	itoa(nr_lectie, str, 10);
	strcat(filename, str);
	strcat(filename, "_");
	if (cv == 1)
		strcat(filename, "ct.txt");
	else
		strcat(filename, "vt.txt");

	ifstream fi(filename);
	ofstream fo("c:\\bc31\\work\\surse\\altele\\italiana\\test_res.txt");
	char cuv_i[35], cuv_r[35];
	int n;

	fi.getline(str, 3);
	cv = atoi(str);

	char c;
	int j;

	gettime(&t1);
	randomize();
	do
	{
		n = random(cv);
		n = n+2;
		fi.close();
		fi.open(filename);
		if (test(n))
		{
			cuv_r[0] = '\0';
			for (int i = 0; i < n; i++)
				fi.getline(cuv_i, 35);
			clrscr();
			x[idx++] = n;
			cout << "\n " << cuv_i << ": ";
			j = 0;
			do
			{
				c = getch();
				if (c == 8)
				{
					if (j > 0)
					{
						gotoxy(wherex()-1, wherey());
						cout << " ";
						gotoxy(wherex()-1, wherey());
						j--;
					}
				}
				else
				{
					cout << c;
					cuv_r[j++] = c;
				}
			}
			while (c != 13);
			cuv_r[j-1] = '\0';
			fo << cuv_i << " - " << cuv_r << "\n";
		}
	}
	while (idx < cv);
	gettime(&t2);

	int sec, min, hour;

	if (t2.ti_sec >= t1.ti_sec)
		sec = t2.ti_sec-t1.ti_sec;
	else
	{
		sec = 60+t2.ti_sec-t1.ti_sec;
		int aux = t2.ti_min;
		aux--;
		if (aux == -1)
		{
			aux = 59;
			int aux2 = t2.ti_hour;
			aux2--;
			if (aux2 == -1)
				aux2 = 23;
			t2.ti_hour = aux2;
		}
		t2.ti_min = aux;
	}

	if (t2.ti_min >= t1.ti_min)
		min = t2.ti_min-t1.ti_min;
	else
	{
		min = 60+t2.ti_min-t1.ti_min;
		int aux = t2.ti_hour;
		aux--;
		if (aux == -1)
			aux = 23;
		t2.ti_hour = aux;
	}

	if (t2.ti_hour >= t2.ti_hour)
		hour = t2.ti_hour-t1.ti_hour;
	else
		hour = 24+t2.ti_hour-t1.ti_hour;

	clrscr();
	cout << "\n Timpul dvs. " << hour << ":" << min << ":" << sec << "."; 

	fo.close();
	fi.close();

	return;
}