/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Sun Oct 27 2002
 * Description:		This is the implementation of the ArrayIndexOutOfBoundsException class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/exceptions/ArrayIndexOutOfBoundsException.h"

ArrayIndexOutOfBoundsException::ArrayIndexOutOfBoundsException(long index)
: FSException()
{
	strstream str;
	str << "An access to an illegal array index (" << index << ") was tried." << ends;
	setMessage(str.str());
	this->index = index;
}

ArrayIndexOutOfBoundsException::ArrayIndexOutOfBoundsException(const char* message, long index)
: FSException(message)
{
	this->index = index;
}

long ArrayIndexOutOfBoundsException::getInvalidIndex()
{
	return index;
}
