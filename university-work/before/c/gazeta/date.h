#if !defined(__DATE_H__)
#define __DATE_H__

#include <stdio.h>
#include <conio.h>
#include <iostream.h>
#include <stdlib.h>
#include <string.h>

typedef char* Date;

const int Days[13] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

const char MonthShort[13][3] = {"", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
										  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

const char MonthLong[13][9] = {"", "January", "February", "March", "April",
										 "May", "June", "July", "August", "September",
										 "October", "November", "December"};

const char DayOfWeek[8][9] = {"", "Sunaday", "Monday", "Tuesday", "Wednesday",
										"Thursday", "Friday", "Saturday"};

const d_Min = 1920;
const d_Max = 9999;

//int d_err;

int   YMD(char *s, int *d_err);
Date  DMY(char *s, int *d_err);
Date  MDY(char *s, int *d_err);

#endif
