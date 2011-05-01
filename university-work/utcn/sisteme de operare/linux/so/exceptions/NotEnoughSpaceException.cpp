/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Fri Nov 8 2002
 * Description:		This is the implementation of the NotEnoughSpaceException class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/exceptions/NotEnoughSpaceException.h"

NotEnoughSpaceException::NotEnoughSpaceException()
: FSException()
{
	strstream str;
	str << "Not enough space left on hard disk to perform operation." << ends;
	setMessage(str.str());
}

NotEnoughSpaceException::NotEnoughSpaceException(const char* message)
: FSException(message)
{}
