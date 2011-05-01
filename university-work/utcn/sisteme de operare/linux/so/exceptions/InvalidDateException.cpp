/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Tue Nov 5 2002
 * Description:		This is the implementation of the InvalidDateException class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/exceptions/InvalidDateException.h"

InvalidDateException::InvalidDateException(const char* date)
: FSException()
{
	strstream str;
	str << "The following date was invalid: " << date << "." << ends;
	setMessage(str.str());
	this->date = new char[strlen(date)+1];
	if (this->date == 0)
	{
		cerr << "Not enough memory.\n";
		exit(1);
	}
	strcpy(this->date, date);
}

InvalidDateException::InvalidDateException(const char* message, const char* date)
: FSException(message)
{
	this->date = new char[strlen(date)+1];
	if (this->date == 0)
	{
		cerr << "Not enough memory.\n";
		exit(1);
	}
	strcpy(this->date, date);
}

InvalidDateException::~InvalidDateException()
{
	if (date != NULL)
	{
		delete [] date;
	}
}

char* InvalidDateException::getInvalidDate()
{
	return date;
}
