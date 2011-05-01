/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Sat Nov 9 2002
 * Description:		This is the implementation of the FileSizeTooLargeException class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/exceptions/FileSizeTooLargeException.h"

FileSizeTooLargeException::FileSizeTooLargeException()
: FSException()
{
	strstream str;
	str << "Inode structure does not allow holding a file that large." << ends;
	setMessage(str.str());
}

FileSizeTooLargeException::FileSizeTooLargeException(const char* message)
: FSException(message)
{}
