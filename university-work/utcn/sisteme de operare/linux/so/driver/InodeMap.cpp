/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Thu Nov 7 2002
 * Description:		This is the implementation of the InodeMap class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/driver/InodeMap.h"

InodeMap::InodeMap()
{
	checksum = new unsigned char[16];
	if (checksum == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	memset(checksum, 0, 16);
	nrInodes = 0;
	bitmap = new unsigned char[BLOCK_DIM-20];
	if (bitmap == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	memset(bitmap, 0, BLOCK_DIM-20);
}

InodeMap::~InodeMap()
{
	if (checksum != NULL)
		delete [] checksum;
	if (bitmap != NULL)
		delete [] bitmap;
}

unsigned char* InodeMap::getChecksum()
{
	return checksum;
}

unsigned long InodeMap::getNrOfInodes()
{
	return nrInodes;
}

void InodeMap::setChecksum(unsigned char* checksum)
{
	if (checksum == NULL)
	{
		if (this->checksum != NULL)
		{
			delete [] this->checksum;
			this->checksum = NULL;
		}
	}
	else
	{
		if (this->checksum == NULL)
		{
			this->checksum = new unsigned char[16];
			if (this->checksum == NULL)
			{
				cout << "Not enough memory.\n";
				exit(1);
			}
		}
		memcpy(this->checksum, checksum, 16);
	}
}

void InodeMap::setNrOfInodes(unsigned long nrOfInodes)
{
	this->nrInodes = nrOfInodes;
}

bool InodeMap::isFree(unsigned long index) throw(ArrayIndexOutOfBoundsException*)
{
	if (index >= 8*(BLOCK_DIM-20))
	{
		throw new ArrayIndexOutOfBoundsException(index);
	}

	// get the byte in which the bit we are looking for resides
	long offset = index % 8;
	index -= offset;
	unsigned char byte = bitmap[index/8];

	// bring the requested bit in the first position: binary - <bit>*******.
	byte <<= offset;

	// mask out the other bits...
	byte &= 0x80;

	// see what we're left with
	if (byte == 0)
		return true;
	else
		return false;
}

void InodeMap::allocInode(unsigned long index) throw(ArrayIndexOutOfBoundsException*)
{
	if (index >= 8*(BLOCK_DIM-20))
	{
		throw new ArrayIndexOutOfBoundsException(index);
	}

	// get the byte in which the bit we are looking for resides
	long offset = index % 8;
	index -= offset;
	unsigned char byte = bitmap[index/8];

	// create mask by shifting 1 (0000 0001) left 7-offset positions
	unsigned char mask = 1;
	mask <<= 7-offset;

	// make bit 1
	byte |= mask;

	bitmap[index/8] = byte;
}

void InodeMap::freeInode(unsigned long index)
{
	if (index >= 8*(BLOCK_DIM-20))
	{
		throw new ArrayIndexOutOfBoundsException(index);
	}

	// get the byte in which the bit we are looking for resides
	long offset = index % 8;
	index -= offset;
	unsigned char byte = bitmap[index/8];

	// create mask by shifting 1 (0000 0001) left 7-offset positions
	unsigned char mask = 1;
	mask <<= 7-offset;
	mask = ~mask;

	// make bit 0
	byte &= mask;

	bitmap[index/8] = byte;
}

unsigned long InodeMap::getFreeBit()
{
	for (unsigned int i = 0; i < ((unsigned int) nrInodes/8); i++)
	{
		if (bitmap[i] != 0xff)
		{
			unsigned char mask = 0x80;
			for (int j = 0; j < 8; j++)
			{
				if ((bitmap[i] & mask) == 0)
					return i*8+j;
				mask >>= 1;
			}
		}
	}

	if ((nrInodes % 8) != 0)
	{
		unsigned char mask = 0x80;
		for (unsigned int i = 0; i < nrInodes % 8; i++)
		{
			if ((bitmap[(int) nrInodes/8] & mask) == 0)
				return ((int) nrInodes/8)*8+i;
			mask >>= 1;
		}
	}

	// if we failed to find a free inode, the method contract
	// specifies that we have to return this value...
	return BLOCK_DIM-20;
}

void InodeMap::readFromHardDisk(HDD* hdd, unsigned long blockNo)
	throw(HardDiskNotInitializedException*, IOException*, InvalidBlockNumberException*)
{
	// temporary buffers
	unsigned char buffer[4];
	unsigned char* data = new unsigned char[BLOCK_DIM];

	if (data == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}

	// read block blockNo
	hdd->readHDDBlock(blockNo, data);

	// checksum has 16 bytes
	if (checksum == NULL)
	{
		checksum = new unsigned char[16];
		if (checksum == NULL)
		{
			cout << "Not enough memory.\n";
			exit(1);
		}
	}
	memcpy(checksum, data, 16);

	// nr. of inodes has 4 bytes
	memcpy(buffer, data+16, 4);
	nrInodes = Utils::bytesToULong(buffer);

	// bitmap has include/defs.h::BLOCK_DIM-20 bytes
	if (bitmap == NULL)
	{	
		bitmap = new unsigned char[BLOCK_DIM-20];
		if (bitmap == NULL)
		{
			cout << "Not enough memory.\n";
			exit(1);
		}
	}
	memcpy(bitmap, data+20, BLOCK_DIM-20);

	if (data != NULL)
	{
		delete [] data;
	}
}
	
void InodeMap::writeToHardDisk(HDD* hdd, unsigned long blockNo)
	throw(HardDiskNotInitializedException*, IOException*, InvalidBlockNumberException*)
{
	unsigned char* data = serialize();
	hdd->writeHDDBlock(blockNo, data);
	if (data != NULL)
	{
		delete [] data;
	}
}

unsigned char* InodeMap::getAsBytes()
{
	return serialize();
}

unsigned char* InodeMap::serialize()
{
	unsigned char* data = new unsigned char[BLOCK_DIM];

	if (data == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}

	// checksum has 16 bytes
	if (checksum != NULL)
	{
		memcpy(data, checksum, 16);
	}
	else
	{
		memset(data, 0, 16);
	}

	// nr. of inodes has 4 bytes
	memcpy(data+16, Utils::uLongToBytes(nrInodes), 4);

	// bitmap has include/defs.h::BLOCK_DIM-20 bytes
	if (bitmap != NULL)
	{
		memcpy(data+20, bitmap, BLOCK_DIM-20);
	}
	else
	{
		memset(data+20, 0, BLOCK_DIM-20);
	}

	return data;
}
