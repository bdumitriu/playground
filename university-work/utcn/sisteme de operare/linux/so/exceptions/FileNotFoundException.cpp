/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Sun Nov 3 2002
 * Description:		This is the implementation of the FileNotFoundException class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/exceptions/FileNotFoundException.h"

FileNotFoundException::FileNotFoundException(const char* fileName)
: FSException()
{
	strstream str;
	str << "File " << fileName << " could not be found." << ends;
	setMessage(str.str());
	this->fileName = new char[strlen(fileName)+1];
	if (this->fileName == 0)
	{
		cerr << "Not enough memory.\n";
		exit(1);
	}
	strcpy(this->fileName, fileName);
}

FileNotFoundException::FileNotFoundException(const char* message, const char* fileName)
: FSException(message)
{
	this->fileName = new char[strlen(fileName)+1];
	if (this->fileName == 0)
	{
		cerr << "Not enough memory.\n";
		exit(1);
	}
	strcpy(this->fileName, fileName);
}

FileNotFoundException::~FileNotFoundException()
{
	if (fileName != NULL)
	{
		delete [] fileName;
	}
}

char* FileNotFoundException::getInvalidFileName()
{
	return fileName;
}
