/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Mon Nov 4 2002
 * Description:		This is the implementation of the Utils class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/utils/Utils.h"

unsigned long Utils::bytesToULong(unsigned char bytes[4])
{
	BytesULong* tmp = new BytesULong;
	memcpy(tmp->bytes, bytes, 4);
	unsigned long retVal = tmp->number;
	delete tmp;
	return retVal;
}

unsigned char* Utils::uLongToBytes(unsigned long number)
{
	BytesULong* tmp = new BytesULong;
	tmp->number = number;
	unsigned char* retVal = new unsigned char[4];
	if (retVal == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	memcpy(retVal, tmp->bytes, 4);
	delete tmp;
	return retVal;
}

unsigned short Utils::bytesToUShort(unsigned char bytes[2])
{
	BytesUShort* tmp = new BytesUShort;
	memcpy(tmp->bytes, bytes, 2);
	unsigned short retVal = tmp->number;
	delete tmp;
	return retVal;
}

unsigned char* Utils::uShortToBytes(unsigned short number)
{
	BytesUShort* tmp = new BytesUShort;
	tmp->number = number;
	unsigned char* retVal = new unsigned char[2];
	if (retVal == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	memcpy(retVal, tmp->bytes, 2);
	delete tmp;
	return retVal;
}
