/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Mon Nov 4 2002
 * Description:		This is the implementation of the NullPointerException class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/exceptions/NullPointerException.h"

NullPointerException::NullPointerException(const char* methodName)
: FSException()
{
	strstream str;
	str << "Somebody tried to use a NULL pointer in method " << methodName << "." << ends;
	setMessage(str.str());
	this->methodName = new char[strlen(methodName)+1];
	if (this->methodName == 0)
	{
		cerr << "Not enough memory.\n";
		exit(1);
	}
	strcpy(this->methodName, methodName);
}

NullPointerException::NullPointerException(const char* message, const char* methodName)
: FSException(message)
{
	this->methodName = new char[strlen(methodName)+1];
	if (this->methodName == 0)
	{
		cerr << "Not enough memory.\n";
		exit(1);
	}
	strcpy(this->methodName, methodName);
}

NullPointerException::~NullPointerException()
{
	if (methodName != NULL)
	{
		delete [] methodName;
	}
}

char* NullPointerException::getMethodName()
{
	return methodName;
}
