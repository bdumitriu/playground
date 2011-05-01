/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Mon Nov 4 2002
 * Description:		This is the implementation of the HardDiskNotInitializedException class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/exceptions/HardDiskNotInitializedException.h"

HardDiskNotInitializedException::HardDiskNotInitializedException()
: FSException()
{
	strstream str;
	str << "Hard disk has not been successfully initialized yet." << ends;
	setMessage(str.str());
}

HardDiskNotInitializedException::HardDiskNotInitializedException(const char* message)
: FSException(message)
{}
