/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Mon Nov 4 2002
 * Description:		This is the implementation of the HDDDriverCache class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/driver/HDDDriverCache.h"

HDDDriverCache::HDDDriverCache(HDDDriver* hdd)
{
	this->hdd = hdd;
}

HDDDriverCache::~HDDDriverCache()
{
// should a "delete hdd;" be here or not? this is the question... probably not!
}

void HDDDriverCache::readHDDBlock(unsigned long blockNo, unsigned char* data)
	throw(InvalidBlockNumberException*, HardDiskNotInitializedException*, IOException*)
{
	hdd->readHDDBlock(blockNo, data);
}

void HDDDriverCache::writeHDDBlock(unsigned long blockNo, unsigned char* data)
	throw(InvalidBlockNumberException*, HardDiskNotInitializedException*, IOException*)
{
	hdd->writeHDDBlock(blockNo, data);
}
