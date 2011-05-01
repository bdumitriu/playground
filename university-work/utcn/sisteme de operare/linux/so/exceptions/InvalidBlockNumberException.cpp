/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Sun Nov 3 2002
 * Description:		This is the implementation of the InvalidBlockNumberException class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/exceptions/InvalidBlockNumberException.h"

InvalidBlockNumberException::InvalidBlockNumberException(long blockNo)
: FSException()
{
	strstream str;
	str << "An access to an invalid block number (" << blockNo << ") was tried." << ends;
	setMessage(str.str());
	this->blockNo = blockNo;
}

InvalidBlockNumberException::InvalidBlockNumberException(const char* message, long blockNo)
: FSException(message)
{
	this->blockNo = blockNo;
}

long InvalidBlockNumberException::getInvalidBlockNo()
{
	return blockNo;
}
