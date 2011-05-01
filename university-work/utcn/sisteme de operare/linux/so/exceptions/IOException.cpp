/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Mon Nov 4 2002
 * Description:		This is the implementation of the IOException class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/exceptions/IOException.h"

IOException::IOException()
: FSException()
{
	strstream str;
	str << "An I/O exception has occured while trying to access the hard disk." << ends;
	setMessage(str.str());
}

IOException::IOException(const char* message)
: FSException(message)
{}
