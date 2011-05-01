/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Tue Nov 5 2002
 * Description:		This is the implementation of the Date class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/utils/Date.h"

bool dateOk(unsigned char day, unsigned char month, unsigned short year);

Date::Date()
{
	// get time (in seconds) elapsed since January 1, 1970
	time_t* currentTime = new time_t;
	time(currentTime);

	// convert it into a more accesible form
	struct tm* currentDate = localtime(currentTime);

	// initialize the date members
	year = currentDate->tm_year+1900;
	month = currentDate->tm_mon+1;
	day = currentDate->tm_mday;
}

Date::Date(const Date& date)
{
	this->day = date.day;
	this->month = date.month;
	this->year = date.year;
}

Date::Date(unsigned char day, unsigned char month, unsigned short year)
	throw(InvalidDateException*)
{
	if (!dateOk(day, month, year))
	{
		// set time to current time and throw exception

		// get time (in seconds) elapsed since January 1, 1970
		time_t* currentTime = new time_t;
		time(currentTime);

		// convert it into a more accesible form
		struct tm* currentDate = gmtime(currentTime);

		// initialize the date members
		this->year = currentDate->tm_year+1900;
		this->month = currentDate->tm_mon+1;
		this->day = currentDate->tm_mday;

		strstream str;
		str << (int) day << "/" << (int) month << "/" << year << ends;
		throw new InvalidDateException(str.str());
	}
	this->day = day;
	this->month = month;
	this->year = year;
}

unsigned char Date::getDay()
{
	return day;
}

unsigned char Date::getMonth()
{
	return month;
}

unsigned short Date::getYear()
{
	return year;
}

void Date::setDay(unsigned char day) throw(InvalidDateException*)
{
	if (!dateOk(day, month, year))
	{
		strstream str;
		str << (int) day << "/" << (int) month << "/" << year << ends;
		throw new InvalidDateException(str.str());
	}
	this->day = day;
}

void Date::setMonth(unsigned char month) throw(InvalidDateException*)
{
	if (!dateOk(day, month, year))
	{
		strstream str;
		str << (int) day << "/" << (int) month << "/" << year << ends;
		throw new InvalidDateException(str.str());
	}
	this->month = month;
}

void Date::setYear(unsigned short year) throw(InvalidDateException*)
{
	if (!dateOk(day, month, year))
	{
		strstream str;
		str << (int) day << "/" << (int) month << "/" << year << ends;
		throw new InvalidDateException(str.str());
	}
	this->year = year;
}

unsigned char* Date::getAsBytes()
{
	unsigned char* retVal = new unsigned char[4];
	if (retVal == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	retVal[0] = day;
	retVal[1] = month;
	memcpy(retVal+2, Utils::uShortToBytes(year), 2);

	return retVal;
}

void Date::setFromBytes(unsigned char* bytes) throw(InvalidDateException*)
{
	if (!dateOk(bytes[0], bytes[1], Utils::bytesToUShort(bytes+2)))
	{
		strstream str;
		str << (int) bytes[0] << "/" << (int) bytes[1] << "/"
			<< Utils::bytesToUShort(bytes+2) << ends;
		throw new InvalidDateException(str.str());
	}
	day = bytes[0];
	month = bytes[1];
	year = Utils::bytesToUShort(bytes+2);
}

bool dateOk(unsigned char day, unsigned char month, unsigned short year)
{
	switch (month)
	{
		case 1:
		case 3:
		case 5:
		case 7:
		case 8:
		case 10:
		case 12:
		{
			if (day > 31)
			{
				return false;
			}
			break;
		}
		case 4:
		case 6:
		case 9:
		case 11:
		{
			if (day > 30)
			{
				return false;
			}
			break;
		}
		case 2:
		{
			if (((year % 4 == 0) && (day > 29)) ||
				((year % 4 != 0) && (day > 28)))
			{
				return false;
			}
			break;
		}
		default:
		{
			return false;
		}
	}
	return true;
}

char* Date::toString()
{
/*	strstream str;
	str << (int) day << "/" << (int) month << "/" << year << ends;
	return str.str();*/

	char* retVal = new char[11];
	if (retVal == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	sprintf(retVal, "%d/%d/%d", day, month, year);
	return retVal;
}
