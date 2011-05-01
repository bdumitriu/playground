/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Sat Nov 2 2002
 * Description:		This is the implementation of the FSException class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/exceptions/FSException.h"

FSException::FSException()
{
	message = new char[strlen("An exception has occured in the file system.")+1];
	if (message == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	strcpy(message, "An exception has occured in the file system.");
}

FSException::FSException(const char* message)
{
	this->message = new char[strlen(message)+1];
	if (this->message == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	strcpy(this->message, message);
}

FSException::~FSException()
{
	if (message != NULL)
	{
		delete [] message;
	}
}

char* FSException::getMessage()
{
	return message;
}


void FSException::setMessage(const char* message)
{
	if (this->message != NULL)
	{
		delete [] this->message;
	}
	this->message = new char[strlen(message)+1];
	if (this->message == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	strcpy(this->message, message);
}
