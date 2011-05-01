#include "date.h"

int Year(char *s)
{
	char x[4];

	for (int i = 0; i < 4; i++)
	{
		x[i] = s[i];
		if ((x[i] < 48) || (x[i] > 57))
			return 0;
	}
	x[4] = '\0';

	return atoi(x);
}

int Month(char *s)
{
	char x[2];

	for (int i = 4; i < 6; i++)
	{
		x[i-4] = s[i];
		if ((x[i] < 48) || (x[i] > 57))
			return 0;
	}
	x[2] = '\0';

	if ((x[0] == '0') && (x[1] == '0'))
		return -1;

	return atoi(x);
}

int Day(char* s)
{
	char x[2];

	for (int i = 6; i < 8; i++)
	{
		x[i-6] = s[i];
		if ((x[i] < 48) || (x[i] > 57))
			return 0;
	}
	x[2] = '\0';

	if ((x[0] == '0') && (x[1] == '0'))
		return -1;

	return atoi(x);
}

int Leap(int y)
{
	if (!(y%4))
		return 1;
	return 0;
}

int DayInMonth(int y, int m, int d)
{
	if (Days[m] < d)
		return 0;
	if ((m == 2) && (d == 29) && (!(Leap(y))))
		return 0;

	return 1;
}

int YMD(char *s, int *d_err)
{
	int year, month, day;
	int test = 1;

	*d_err = 0;

	year = Year(s);  // verificarea anului
	if (year == 0)
	{
		*d_err += 1;
		test = 0;
	}
	if ((year < d_Min) && (test == 1))
	{
		*d_err += 2;
		test = 0;
	}

	month = Month(s);  // verificarea lunii
	if (month == 0)
	{
		*d_err += 4;
		test = 0;
	}
	if (month < 0)
	{
		*d_err += 8;
		test = 0;
	}
	if (month > 12)
	{
		*d_err += 16;
		test = 0;
	}

	day = Day(s);  // verificarea zilei
	if (day == 0)
	{
		*d_err += 32;
		test = 0;
	}
	if (day < 0)
	{
		*d_err += 64;
		test = 0;
	}
	if (test == 1)  // verificarea (test = 1) trebuie facuta deoarece functia
		if (!(DayInMonth(year, month, day)))  // DayInMonth foloseste variabi-
		{                          // lele year, month si day deci daca acestea
			*d_err += 128;        // nu au fost convertite corect va da eroare.
			test = 0;
		}

	if (!(test))
		return 0;

	return 1;
}