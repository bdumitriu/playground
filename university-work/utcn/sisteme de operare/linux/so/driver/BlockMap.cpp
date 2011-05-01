/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Sat Nov 9 2002
 * Description:		This is the implementation of the BlockMap class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/driver/BlockMap.h"

BlockMap::BlockMap(HDD* hdd)
{
	this->hdd = hdd;
	fbInode = new Inode();
	fbInode->readFromHardDisk(hdd, FILEATTR_AREA_START_ADDR+FREEBLOCKS_FILEATTR);
	fis = new FileInputStream(hdd, fbInode);
	fis->enableBuffering();
	fos = new FileOutputStream(hdd, fbInode, FILEATTR_AREA_START_ADDR+FREEBLOCKS_FILEATTR, this);
}

BlockMap::~BlockMap()
{
	if (fis != NULL)
		delete fis;
	if (fbInode != NULL)
		delete fbInode;
}

unsigned char* BlockMap::getChecksum()
	throw(HardDiskNotInitializedException*, IOException*, InvalidBlockNumberException*)
{
	unsigned char* checksum = new unsigned char[16];
	if (checksum == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}

	fis->setPointer(0);
	fis->read(checksum, 16);

	return checksum;
}

unsigned long BlockMap::getNrOfBlocks()
	throw(HardDiskNotInitializedException*, IOException*, InvalidBlockNumberException*)
{
	unsigned char* nrBlocks = new unsigned char[16];
	if (nrBlocks == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}

	fis->setPointer(16);
	fis->read(nrBlocks, 4);

	return Utils::bytesToULong(nrBlocks);
}

void BlockMap::setChecksum(unsigned char* checksum)
	throw(HardDiskNotInitializedException*, IOException*, InvalidBlockNumberException*)
{

	fos->setPointer(0);
	fos->write(checksum, 16);
	fis->invalidateBuffer();
}

void BlockMap::setNrOfBlocks(unsigned long nrOfBlocks)
	throw(HardDiskNotInitializedException*, IOException*, InvalidBlockNumberException*)
{

	fos->setPointer(16);
	fos->write(Utils::uLongToBytes(nrOfBlocks), 4);
	fis->invalidateBuffer();
}

bool BlockMap::isFree(unsigned long index) throw(
	ArrayIndexOutOfBoundsException*,
	HardDiskNotInitializedException*,
	IOException*,
	InvalidBlockNumberException*)
{
	fis->setPointer(20+((unsigned long) index/8));
	unsigned char byte = fis->read();

	long offset = index % 8;

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

void BlockMap::allocBlock(unsigned long index) throw(
	ArrayIndexOutOfBoundsException*,
	HardDiskNotInitializedException*,
	IOException*,
	InvalidBlockNumberException*)
{
	// read the byte
	fis->setPointer(20+((unsigned long) index/8));
	unsigned char byte = fis->read();

	// modify the necessary bit
	long offset = index % 8;
	unsigned char mask = 0x80;
	mask >>= offset;
	byte |= mask;

	// write the byte back
	fos->setPointer(20+((unsigned long) index/8));
	fos->write(&byte, 1);

	fis->invalidateBuffer();
}

void BlockMap::allocBlocks(unsigned long* blocks, unsigned long nr) throw(
	ArrayIndexOutOfBoundsException*,
	HardDiskNotInitializedException*,
	IOException*,
	InvalidBlockNumberException*)
{
	for (unsigned long i = 0; i < nr; i++)
	{
		allocBlock(blocks[i]);
	}

	/* when you reimplement the method more efficiently, don't */
	/* forget to call fis->invalidateBuffer() in the end !!!!! */
}

void BlockMap::freeBlock(unsigned long index) throw(
	ArrayIndexOutOfBoundsException*,
	HardDiskNotInitializedException*,
	IOException*,
	InvalidBlockNumberException*)
{
	// read the byte
	fis->setPointer(20+((unsigned long) index/8));
	unsigned char byte = fis->read();

	// modify the necessary bit
	long offset = index % 8;
	unsigned char mask = 0x80;
	mask >>= offset;
	mask = ~mask;
	byte &= mask;

	// write the byte back
	fos->setPointer(20+((unsigned long) index/8));
	fos->write(&byte, 1);

	fis->invalidateBuffer();
}

/*
 * Returns the position of the first bit which is 0 in the byte
 * received as parameter. If all bits are 1 (i.e. byte = 0xff),
 * the function returns 8. Counting starts with 0 from the msb.
 */
unsigned long getFirstZeroBit(unsigned char byte)
{
	unsigned char mask = 0x80;
	for (unsigned long i = 0; i < 8; i++)
	{
		if ((byte & mask) == 0)
			return i;
		mask >>= 1;
	}
	return 8;
}


unsigned long BlockMap::getFreeBlock()
	throw(HardDiskNotInitializedException*, IOException*, InvalidBlockNumberException*)
{
	// temporary buffer
	unsigned char* data = new unsigned char[BLOCK_DIM];
	if (data == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}

	// the total number of bits to look through
	unsigned long n = getNrOfBlocks();

	fis->setPointer(20);
	fis->read(data, BLOCK_DIM-20);

	// look through the bits in the first block
	// (the one who has its first 20 bytes occupied with the
	// chesksum and the number of non system blocks
	if (n <= (BLOCK_DIM-20)*8)
	{
		for (unsigned long i = 0; i < ((unsigned long) n/8); i++)
		{
			if (data[i] != 0xff)
			{
				unsigned long firstZeroBitPos = getFirstZeroBit(data[i]);
				if (data != NULL)
					delete [] data;
				return i*8+firstZeroBitPos;
			}
		}
		if ((n % 8) != 0)
		{
			unsigned char mask = 0x80;
			for (unsigned long i = 0; i < n % 8; i++)
			{
				if ((data[(unsigned long) n/8] & mask) == 0)
				{
					if (data != NULL)
						delete [] data;
					return ((unsigned long) n/8)*8+i;
				}
				mask >>= 1;
			}
		}
		if (data != NULL)
			delete [] data;
		return getNrOfBlocks();
	}
	else
	{
		for (unsigned long i = 0; i < BLOCK_DIM-20; i++)
		{
			if (data[i] != 0xff)
			{
				unsigned long firstZeroBitPos = getFirstZeroBit(data[i]);
				if (data != NULL)
					delete [] data;
				return i*8+firstZeroBitPos;
			}
		}
	}

	n -= (BLOCK_DIM-20)*8;

	// number of iterations
	unsigned long it = 0;

	// look through the bits in the other blocks
	while (n > 0)
	{
		it++;

		// read the next block of data...
		fis->setPointer(BLOCK_DIM*it);
		fis->read(data, BLOCK_DIM);

		// ...and look through it
		if (n <= BLOCK_DIM*8)
		{
			for (unsigned long i = 0; i < ((unsigned long) n/8); i++)
			{
				if (data[i] != 0xff)
				{
					unsigned long firstZeroBitPos = getFirstZeroBit(data[i]);
					if (data != NULL)
						delete [] data;
					return (BLOCK_DIM*it-20+i)*8+firstZeroBitPos;
				}
			}
			if ((n % 8) != 0)
			{
				unsigned char mask = 0x80;
				for (unsigned long i = 0; i < n % 8; i++)
				{
					if ((data[(unsigned long) n/8] & mask) == 0)
						return (BLOCK_DIM*it-20+((unsigned long) n/8))*8+i;
					mask >>= 1;
				}
			}
			if (data != NULL)
				delete [] data;
			return getNrOfBlocks();
		}
		else
		{
			for (unsigned long i = 0; i < BLOCK_DIM; i++)
			{
				if (data[i] != 0xff)
				{
					unsigned long firstZeroBitPos = getFirstZeroBit(data[i]);
					if (data != NULL)
						delete [] data;
					return (BLOCK_DIM*it-20+i)*8+firstZeroBitPos;
				}
			}
		}

		n -= BLOCK_DIM*8;
	}

	// this can never be reached, but i got sick of getting
	// warnings like: "control reaches end of non-void function"
	return getNrOfBlocks();
}

unsigned long BlockMap::getFreeBlocks(unsigned long* freeBlocks, unsigned long nr)
	throw(HardDiskNotInitializedException*, IOException*, InvalidBlockNumberException*)
{
	// avoid stupid requests...
	if (nr == 0)
		return 0;

	// temporary buffer
	unsigned char* data = new unsigned char[BLOCK_DIM];
	if (data == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}

	// the index in the freeBlocks array
	unsigned long idx = 0;

	// the total number of bits to look through
	unsigned long n = getNrOfBlocks();

	fis->setPointer(20);
	fis->read(data, BLOCK_DIM-20);

	// look through the bits in the first block
	// (the one who has its first 20 bytes occupied with the
	// chesksum and the number of non system blocks
	if (n <= (BLOCK_DIM-20)*8)
	{
		for (unsigned long i = 0; i < ((unsigned long) n/8); i++)
		{
			if (data[i] != 0xff)
			{
				unsigned char mask = 0x80;
				for (unsigned long j = 0; j < 8; j++)
				{
					if ((data[i] & mask) == 0)
					{
						freeBlocks[idx++] = i*8+j;
						if (idx == nr)
						{
							if (data != NULL)
								delete [] data;
							return idx;
						}
					}
					mask >>= 1;
				}
			}
		}
		if ((n % 8) != 0)
		{
			unsigned char mask = 0x80;
			for (unsigned long i = 0; i < n % 8; i++)
			{
				if ((data[(unsigned long) n/8] & mask) == 0)
				{
					freeBlocks[idx++] = ((unsigned long) n/8)*8+i;
					if (idx == nr)
					{
						if (data != NULL)
							delete [] data;
						return idx;
					}
				}
				mask >>= 1;
			}
		}
		if (data != NULL)
			delete [] data;
		return idx;
	}
	else
	{
		for (unsigned long i = 0; i < BLOCK_DIM-20; i++)
		{
			if (data[i] != 0xff)
			{
				unsigned char mask = 0x80;
				for (unsigned long j = 0; j < 8; j++)
				{
					if ((data[i] & mask) == 0)
					{
						freeBlocks[idx++] = i*8+j;
						if (idx == nr)
						{
							if (data != NULL)
								delete [] data;
							return idx;
						}
					}
					mask >>= 1;
				}
			}
		}
	}

	n -= (BLOCK_DIM-20)*8;

	// number of iterations
	unsigned long it = 0;

	// look through the bits in the other blocks
	while (n > 0)
	{
		it++;

		// read the next block of data...
		fis->setPointer(BLOCK_DIM*it);
		fis->read(data, BLOCK_DIM);

		// ...and look through it
		if (n <= BLOCK_DIM*8)
		{
			for (unsigned long i = 0; i < ((unsigned long) n/8); i++)
			{
				if (data[i] != 0xff)
				{
					unsigned char mask = 0x80;
					for (unsigned long j = 0; j < 8; j++)
					{
						if ((data[i] & mask) == 0)
						{
							freeBlocks[idx++] = (BLOCK_DIM*it-20+i)*8+j;
							if (idx == nr)
							{
								if (data != NULL)
									delete [] data;
								return idx;
							}
						}
						mask >>= 1;
					}
				}
			}
			if ((n % 8) != 0)
			{
				unsigned char mask = 0x80;
				for (unsigned long i = 0; i < n % 8; i++)
				{
					if ((data[(unsigned long) n/8] & mask) == 0)
					{
						freeBlocks[idx++] = (BLOCK_DIM*it-20+((unsigned long) n/8))*8+i;
						if (idx == nr)
						{
							if (data != NULL)
								delete [] data;
							return idx;
						}
					}
					mask >>= 1;
				}
			}
			if (data != NULL)
				delete [] data;
			return idx;
		}
		else
		{
			for (unsigned long i = 0; i < BLOCK_DIM; i++)
			{
				if (data[i] != 0xff)
				{
					unsigned char mask = 0x80;
					for (unsigned long j = 0; j < 8; j++)
					{
						if ((data[i] & mask) == 0)
						{
							freeBlocks[idx++] = (BLOCK_DIM*it-20+i)*8+j;
							if (idx == nr)
							{
								if (data != NULL)
									delete [] data;
								return idx;
							}
						}
						mask >>= 1;
					}
				}
			}
		}

		n -= BLOCK_DIM*8;
	}

	// this can never be reached, but i got sick of getting
	// warnings like: "control reaches end of non-void function"
	return 0;	
}
