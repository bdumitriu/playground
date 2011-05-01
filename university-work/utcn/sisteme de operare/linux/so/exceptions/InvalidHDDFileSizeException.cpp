/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Sun Nov 3 2002
 * Description:		This is the implementation of the InvalidHDDFileSizeException class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/exceptions/InvalidHDDFileSizeException.h"

InvalidHDDFileSizeException::InvalidHDDFileSizeException(unsigned long size)
: IOException()
{
	strstream str;
	str << "The hard disk file size (" << size << " bytes) is invalid." << ends;
	setMessage(str.str());
	this->size = size;
}

InvalidHDDFileSizeException::InvalidHDDFileSizeException(const char* message, unsigned long size)
: IOException(message)
{
	this->size = size;
}

unsigned long InvalidHDDFileSizeException::getInvalidSize()
{
	return size;
}
