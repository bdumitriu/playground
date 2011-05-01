/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Mon Nov 4 2002
 * Description:		This is the implementation of the Superblock class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/driver/Superblock.h"

Superblock::Superblock()
{
	checksum = new unsigned char[16];
	if (checksum == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	memset(checksum, 0, 16);
	signature = new unsigned char[12];
	if (signature == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	memset(signature, 0, 12);
	nrBlocks = 0;
	blockDim = 0;
	inodeStart = 0;
	inodeEnd = 0;
	roodDir = 0;
	freeBlocks = 0;
}

Superblock::~Superblock()
{
	if (checksum != NULL)
	{
		delete [] checksum;
	}
	if (signature != NULL)
	{
		delete [] signature;
	}
}

unsigned char* Superblock::getChecksum()
{
	return checksum;
}
	
unsigned char* Superblock::getSignature()
{
	return signature;
}

unsigned long Superblock::getNrOfBlocks()
{
	return nrBlocks;
}

unsigned long Superblock::getBlockDimension()
{
	return blockDim;
}
	
unsigned long Superblock::getFirstInodeBlock()
{
	return inodeStart;
}
	
unsigned long Superblock::getLastInodeBlock()
{
	return inodeEnd;
}
	
unsigned long Superblock::getRootDirInode()
{
	return roodDir;
}
	
unsigned long Superblock::getFreeBlocksInode()
{
	return freeBlocks;
}

void Superblock::setChecksum(const unsigned char* checksum)
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

void Superblock::setSignature(const unsigned char* signature)
{
	if (signature == NULL)
	{
		if (this->signature != NULL)
		{
			delete [] this->signature;
			this->signature = NULL;
		}
	}
	else
	{
		if (this->signature == NULL)
		{
			this->signature = new unsigned char[12];
			if (this->signature == NULL)
			{
				cout << "Not enough memory.\n";
				exit(1);
			}
		}
		memcpy(this->signature, signature, 12);
	}
}

void Superblock::setNrOfBlocks(unsigned long nrOfBlocks)
{
	nrBlocks = nrOfBlocks;
}

void Superblock::setBlockDimension(unsigned long blockDimension)
{
	blockDim = blockDimension;
}

void Superblock::setFirstInodeBlock(unsigned long firstInodeBlock)
{
	inodeStart = firstInodeBlock;
}

void Superblock::setLastInodeBlock(unsigned long lastInodeBlock)
{
	inodeEnd = lastInodeBlock;
}

void Superblock::setRootDirInode(unsigned long rootDirInode)
{
	roodDir = rootDirInode;
}

void Superblock::setFreeBlocksInode(unsigned long freeBlocksInode)
{
	freeBlocks = freeBlocksInode;
}

void Superblock::readFromHardDisk(HDD* hdd) throw(HardDiskNotInitializedException*, IOException*)
{
	try
	{
		// temporary buffers
		unsigned char* data = new unsigned char[BLOCK_DIM];
		unsigned char buffer[4];

		if (data == NULL)
		{
			cout << "Not enough memory.\n";
			exit(1);
		}

		// superblock is always block 0
		hdd->readHDDBlock(0, data);

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

		// signature has 12 bytes
		if (signature == NULL)
		{
			signature = new unsigned char[12];
			if (signature == NULL)
			{
				cout << "Not enough memory.\n";
				exit(1);
			}
		}
		memcpy(signature, data+16, 12);

		// nrBlocks has 4 bytes
		memcpy(buffer, data+28, 4);
		nrBlocks = Utils::bytesToULong(buffer);

		// blockDim has 4 bytes
		memcpy(buffer, data+32, 4);
		blockDim = Utils::bytesToULong(buffer);

		// inodeStart has 4 bytes
		memcpy(buffer, data+36, 4);
		inodeStart = Utils::bytesToULong(buffer);

		// inodeEnd has 4 bytes
		memcpy(buffer, data+40, 4);
		inodeEnd = Utils::bytesToULong(buffer);		

		// roodDir has 4 bytes
		memcpy(buffer, data+44, 4);
		roodDir = Utils::bytesToULong(buffer);

		// freeBlocks has 4 bytes
		memcpy(buffer, data+48, 4);
		freeBlocks = Utils::bytesToULong(buffer);

		if (data != NULL)
		{
			delete [] data;
		}
	}
	catch (InvalidBlockNumberException* e)
	{
		throw new IOException("Hard disk does not contain superblock.");
	}
}

void Superblock::writeToHardDisk(HDD* hdd)
	throw(HardDiskNotInitializedException*, IOException*)
{
	unsigned char* data = serialize();

	try
	{
		// superblock is always block 0
		hdd->writeHDDBlock(0, data);
	}
	catch (InvalidBlockNumberException* e)
	{
		throw new IOException("Hard disk does not contain superblock.");
	}

	if (data != NULL)
	{
		delete [] data;
	}
}

unsigned char* Superblock::getAsBytes()
{
	return serialize();
}

unsigned char* Superblock::serialize()
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

	// signature has 12 bytes
	if (signature != NULL)
	{
		memcpy(data+16, signature, 12);
	}
	else
	{
		memset(data+16, 0, 12);
	}

	// nrBlocks has 4 bytes
	memcpy(data+28, Utils::uLongToBytes(nrBlocks), 4);

	// blockDim has 4 bytes
	memcpy(data+32, Utils::uLongToBytes(blockDim), 4);

	// inodeStart has 4 bytes
	memcpy(data+36, Utils::uLongToBytes(inodeStart), 4);

	// inodeEnd has 4 bytes
	memcpy(data+40, Utils::uLongToBytes(inodeEnd), 4);

	// roodDir has 4 bytes
	memcpy(data+44, Utils::uLongToBytes(roodDir), 4);

	// freeBlocks has 4 bytes
	memcpy(data+48, Utils::uLongToBytes(freeBlocks), 4);

	// the rest up to BLOCK_DIM are reserved
	memset(data+52, 0, BLOCK_DIM-68);

	return data;
}
