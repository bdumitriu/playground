/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Sun Nov 10 2002
 * Description:		This is the implementation of the FileBlocks class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/driver/FileBlocks.h"

FileBlocks::FileBlocks(HDD* hdd, Inode* inode)
{
	this->hdd = hdd;
	this->inode = inode;
	address = new unsigned char[BLOCK_DIM];
	if (address == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
}

FileBlocks::~FileBlocks()
{
	if (address != NULL)
		delete [] address;
}

unsigned long FileBlocks::getBlock(unsigned long index) throw(
	ArrayIndexOutOfBoundsException*,
	HardDiskNotInitializedException*,
	InvalidBlockNumberException*,
	IOException*)
{
	if (index >= MAX_BLOCKS)
	{
		throw new ArrayIndexOutOfBoundsException(index);
	}

	if (index < DIRECT_BLOCKADDR_ENTRIES)
	{
		return inode->getDirectBlockAddress(index);
	}
	else
	{
		hdd->readHDDBlock(
			inode->getIndirectBlockAddress((index-DIRECT_BLOCKADDR_ENTRIES)/(BLOCK_DIM/4)),
			address);
		return Utils::bytesToULong(address+((index-DIRECT_BLOCKADDR_ENTRIES) % (BLOCK_DIM/4))*4);
	}
}

void FileBlocks::setBlock(unsigned long index, unsigned long blockAddress) throw(
	ArrayIndexOutOfBoundsException*,
	HardDiskNotInitializedException*,
	InvalidBlockNumberException*,
	IOException*)
{
	if (index >= MAX_BLOCKS)
	{
		throw new ArrayIndexOutOfBoundsException(index);
	}

	if (index < DIRECT_BLOCKADDR_ENTRIES)
	{
		inode->setDirectBlockAddress(index, blockAddress);
	}
	else
	{
		hdd->readHDDBlock(
			inode->getIndirectBlockAddress((index-DIRECT_BLOCKADDR_ENTRIES)/(BLOCK_DIM/4)),
			address);
		unsigned char* bytes = Utils::uLongToBytes(blockAddress);
		memcpy(address+((index-DIRECT_BLOCKADDR_ENTRIES) % (BLOCK_DIM/4))*4, bytes, 4);
		hdd->writeHDDBlock(
			inode->getIndirectBlockAddress((index-DIRECT_BLOCKADDR_ENTRIES)/(BLOCK_DIM/4)),
			address);
		if (bytes != NULL)
			delete [] bytes;
	}
}

unsigned long* FileBlocks::getBlocks(unsigned long beginIndex, unsigned long length) throw(
	ArrayIndexOutOfBoundsException*,
	HardDiskNotInitializedException*,
	InvalidBlockNumberException*,
	IOException*)
{
	if ((beginIndex+length) > MAX_BLOCKS)
	{
		throw new ArrayIndexOutOfBoundsException(beginIndex+length);
	}

	unsigned long* addresses = new unsigned long[length];
	if (addresses == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}

	for (unsigned long i = 0; i < length; i++)
	{
		addresses[i] = getBlock(beginIndex+i);
	}

	return addresses;
}

void FileBlocks::setBlocks(unsigned long beginIndex, unsigned long length, unsigned long* addresses) throw(
	ArrayIndexOutOfBoundsException*,
	HardDiskNotInitializedException*,
	InvalidBlockNumberException*,
	IOException*)
{
	if ((beginIndex+length) > MAX_BLOCKS)
	{
		throw new ArrayIndexOutOfBoundsException(beginIndex+length);
	}

	for (unsigned long i = 0; i < length; i++)
	{
		setBlock(beginIndex+i, addresses[i]);
	}
}
