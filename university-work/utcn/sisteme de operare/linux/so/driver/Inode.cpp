/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Wed Nov 6 2002
 * Description:		This is the implementation of the Inode class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/driver/Inode.h"

Inode::Inode()
{
	valid = 0;
	fileType = 0;
	checksum = new unsigned char[16];
	if (checksum == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	memset(checksum, 0, 16);
	fileSize = 0;
	nrBlocks = 0;
	fnSize = 0;
	crDate = new Date();
	if (crDate == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	laDate = new Date();
	if (laDate == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	luDate = new Date();
	if (luDate == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	fileName = new char[128];
	if (fileName == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	memset(fileName, 0, 128);
	dirBlocks = new unsigned long[DIRECT_BLOCKADDR_ENTRIES];
	if (dirBlocks == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);		
	}
	for (int i = 0; i < DIRECT_BLOCKADDR_ENTRIES; i++)
		dirBlocks[i] = 0;
	indirBlocks = new unsigned long[(int) (BLOCK_DIM-180-DIRECT_BLOCKADDR_ENTRIES*4)/4];
	if (indirBlocks == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);		
	}
	for (int i = 0; i < (int) (BLOCK_DIM-180-DIRECT_BLOCKADDR_ENTRIES*4)/4; i++)
		indirBlocks[i] = 0;
}

Inode::~Inode()
{
	if (checksum != NULL)
		delete [] checksum;
	if (crDate != NULL)
		delete crDate;
	if (laDate != NULL)
		delete laDate;
	if (luDate != NULL)
		delete luDate;
	if (fileName != NULL)
		delete [] fileName;
	if (dirBlocks != NULL)
		delete [] dirBlocks;
	if (indirBlocks != NULL)
		delete [] indirBlocks;
}

bool Inode::isValid()
{
	return valid;
}

unsigned char Inode::getFileType()
{
	return fileType;
}

unsigned char* Inode::getChecksum()
{
	return checksum;
}
	
unsigned long Inode::getFileSizeInBytes()
{
	return fileSize;
}

unsigned long Inode::getFileSizeInBlocks()
{
	return nrBlocks;
}

unsigned char Inode::getFileNameSize()
{
	return fnSize;
}

Date* Inode::getCreationDate()
{
	return new Date(*crDate);
}

Date* Inode::getLastAccessDate()
{
	return new Date(*laDate);
}

Date* Inode::getLastUpdateDate()
{
	return new Date(*luDate);
}

char* Inode::getFileName()
{
	return fileName;
}

unsigned long Inode::getDirectBlockAddress(unsigned long index)
	throw(ArrayIndexOutOfBoundsException*)
{
	if (index >= DIRECT_BLOCKADDR_ENTRIES)
	{
		throw new ArrayIndexOutOfBoundsException(index);
	}
	if (dirBlocks != NULL)
		return dirBlocks[index];
	else
		throw new NullPointerException("Inode::getDirectBlockAddress");
}

unsigned long Inode::getIndirectBlockAddress(unsigned long index)
	throw(ArrayIndexOutOfBoundsException*)
{
	if (index >= (BLOCK_DIM-180-DIRECT_BLOCKADDR_ENTRIES*4)/4)
	{
		throw new ArrayIndexOutOfBoundsException(index);
	}
	if (indirBlocks != NULL)
		return indirBlocks[index];
	else
		throw new NullPointerException("Inode::getIndirectBlockAddress");
}

void Inode::setValid(bool valid)
{
	this->valid = valid;
}

void Inode::setFileType(unsigned char fileType)
{
	this->fileType = fileType;
}

void Inode::setChecksum(unsigned char* checksum)
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
	
void Inode::setFileSizeInBytes(unsigned long fileSizeInBytes)
{
	this->fileSize = fileSizeInBytes;
}

void Inode::setFileSizeInBlocks(unsigned long fileSizeInBlocks)
{
	this->nrBlocks = fileSizeInBlocks;
}

void Inode::setFileNameSize(unsigned char fileNameSize)
{
	if (fileNameSize <= 128)
	{
		this->fnSize = fileNameSize;
	}
	else
	{
		this->fnSize = 128;
	}
}

void Inode::setCreationDate(Date* creationDate)
{
	this->crDate = new Date(*creationDate);
}

void Inode::setLastAccessDate(Date* lastAccessDate)
{
	this->laDate = new Date(*lastAccessDate);
}

void Inode::setLastUpdateDate(Date* lastUpdateDate)
{
	this->luDate = new Date(*lastUpdateDate);
}

void Inode::setFileName(const char* fileName, unsigned long fileNameSize)
{
	if (fileName == NULL)
	{
		if (this->fileName != NULL)
		{
			delete [] this->fileName;
			this->fileName = NULL;
		}
	}
	else
	{
		if (this->fileName == NULL)
		{
			this->fileName = new char[128];
			if (this->fileName == NULL)
			{
				cout << "Not enough memory.\n";
				exit(1);
			}
		}
		if (fileNameSize > 128)
		{
			memcpy(this->fileName, fileName, 128);
		}
		else
		{
			memcpy(this->fileName, fileName, fileNameSize);
		}
	}
}

void Inode::setDirectBlockAddress(unsigned long index, unsigned long address)
	throw(ArrayIndexOutOfBoundsException*)
{
	if (index >= DIRECT_BLOCKADDR_ENTRIES)
	{
		throw new ArrayIndexOutOfBoundsException(index);
	}
	if (this->dirBlocks == NULL)
	{
		this->dirBlocks = new unsigned long[DIRECT_BLOCKADDR_ENTRIES];
		if (this->dirBlocks == NULL)
		{
			cout << "Not enough memory.\n";
			exit(1);
		}
	}
	dirBlocks[index] = address;
}

void Inode::setIndirectBlockAddress(unsigned long index, unsigned long address)
	throw(ArrayIndexOutOfBoundsException*)
{
	if (index >= (BLOCK_DIM-180-DIRECT_BLOCKADDR_ENTRIES*4)/4)
	{
		throw new ArrayIndexOutOfBoundsException(index);
	}
	if (this->indirBlocks == NULL)
	{
		this->indirBlocks = new unsigned long[(BLOCK_DIM-180-DIRECT_BLOCKADDR_ENTRIES*4)/4];
		if (this->indirBlocks == NULL)
		{
			cout << "Not enough memory.\n";
			exit(1);
		}
	}
	indirBlocks[index] = address;
}

void Inode::readFromHardDisk(HDD* hdd, unsigned long blockNo) throw(
	HardDiskNotInitializedException*,
	IOException*,
	InvalidBlockNumberException*,
	InvalidDateException*)
{
	// temporary buffers
	unsigned char buffer[4];
	unsigned char* data = new unsigned char[BLOCK_DIM];

	// flag used to throw exception if any of the dates is invalid
	bool allDatesOk = true;

	if (data == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}

	// read block blockNo
	hdd->readHDDBlock(blockNo, data);

	memcpy(buffer, data, 1);

	// first bit = 1 => valid inode
	// first bit = 0 => invalid inode
	if ((buffer[0] & 0x80) == 0)
	{
		valid = false;
	}
	else
	{
		valid = true;
	}

	// file type = the last 7 bits
	fileType = (buffer[0] & 0x7f);

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
	memcpy(checksum, data+1, 16);

	// file size in bytes has 4 bytes
	memcpy(buffer, data+17, 4);
	fileSize = Utils::bytesToULong(buffer);

	// file size in blocks has 4 bytes
	memcpy(buffer, data+21, 4);
	nrBlocks = Utils::bytesToULong(buffer);

	// file name size is 1 byte
	memcpy(buffer, data+25, 1);
	fnSize = buffer[0] < 128 ? buffer[0] : 128;

	try
	{
		// creation date has 4 bytes
		memcpy(buffer, data+40, 4);
		if (crDate == NULL)
		{
			crDate = new Date();
			if (crDate == NULL)
			{
				cout << "Not enough memory,\n";
				exit(1);
			}
		}
		crDate->setFromBytes(buffer);
	}
	catch (InvalidDateException* e)
	{
		if (crDate != NULL)
		{
			delete crDate;
		}
		crDate = new Date();
		if (crDate == NULL)
		{
			cout << "Not enough memory.\n";
			exit(1);
		}

		allDatesOk = false;
	}

	try
	{
		// last access date has 4 bytes
		memcpy(buffer, data+44, 4);
		if (laDate == NULL)
		{
			laDate = new Date();
			if (laDate == NULL)
			{
				cout << "Not enough memory,\n";
				exit(1);
			}
		}
		laDate->setFromBytes(buffer);
	}
	catch (InvalidDateException* e)
	{
		if (laDate != NULL)
		{
			delete laDate;
		}
		laDate = new Date();
		if (laDate == NULL)
		{
			cout << "Not enough memory.\n";
			exit(1);
		}

		allDatesOk = false;
	}

	try
	{
		// last update date has 4 bytes
		memcpy(buffer, data+48, 4);
		if (luDate == NULL)
		{
			luDate = new Date();
			if (luDate == NULL)
			{
				cout << "Not enough memory,\n";
				exit(1);
			}
		}
		luDate->setFromBytes(buffer);
	}
	catch (InvalidDateException* e)
	{
		if (luDate != NULL)
		{
			delete luDate;
		}
		luDate = new Date();
		if (luDate == NULL)
		{
			cout << "Not enough memory.\n";
			exit(1);
		}

		allDatesOk = false;
	}

	// file name has 128 bytes
	if (fileName == NULL)
	{
		fileName = new char[128];
		if (fileName == NULL)
		{
			cout << "Not enough memory.\n";
			exit(1);
		}
	}
	memcpy(fileName, data+52, 128);

	if (dirBlocks == NULL)
	{
		dirBlocks = new unsigned long[DIRECT_BLOCKADDR_ENTRIES];
		if (dirBlocks == NULL)
		{
			cout << "Not enough memory.\n";
			exit(1);
		}
	}
	for (int i = 0; i < DIRECT_BLOCKADDR_ENTRIES; i++)
	{
		// direct block addresses have 4 bytes each
		memcpy(buffer, data+180+(i*4), 4);
		dirBlocks[i] = Utils::bytesToULong(buffer);
	}

	if (indirBlocks == NULL)
	{
		indirBlocks = new unsigned long[(BLOCK_DIM-180-DIRECT_BLOCKADDR_ENTRIES*4)/4];
		if (indirBlocks == NULL)
		{
			cout << "Not enough memory.\n";
			exit(1);
		}
	}
	for (int i = 0; i < (BLOCK_DIM-180-DIRECT_BLOCKADDR_ENTRIES*4)/4; i++)
	{
		// indirect block addresses have 4 bytes each
		memcpy(buffer, data+180+(4*DIRECT_BLOCKADDR_ENTRIES)+(i*4), 4);
		indirBlocks[i] = Utils::bytesToULong(buffer);
	}

	if (data != NULL)
	{
		delete [] data;
	}

	// only throw exception if inode is supposed to be valid
	if (!allDatesOk && valid)
	{
		throw new InvalidDateException("At least one of the dates was invalid.", "");
	}
}

void Inode::writeToHardDisk(HDD* hdd, unsigned long blockNo)
	throw(HardDiskNotInitializedException*, IOException*, InvalidBlockNumberException*)
{
	unsigned char* data = serialize();
	hdd->writeHDDBlock(blockNo, data);
	if (data != NULL)
	{
		delete [] data;
	}
}

unsigned char* Inode::getAsBytes()
{
	return serialize();
}

unsigned char* Inode::serialize()
{
	unsigned char* data = new unsigned char[BLOCK_DIM];

	if (data == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}

	// create byte composed of 1 validity bit & 7 file type bits
	unsigned char tmp = fileType;
	if (valid)
	{
		tmp = tmp | 0x80;
	}
	else
	{
		tmp = tmp & 0x7f;
	}
	memcpy(data, &tmp, 1);

	// checksum has 16 bytes
	if (checksum != NULL)
	{
		memcpy(data+1, checksum, 16);
	}
	else
	{
		memset(data+1, 0, 16);
	}

	// file size in bytes has 4 bytes
	memcpy(data+17, Utils::uLongToBytes(fileSize), 4);

	// file size in blocks has 4 bytes
	memcpy(data+21, Utils::uLongToBytes(nrBlocks), 4);

	// file name size has 1 byte
	memcpy(data+25, &fnSize, 1);

	// creation date has 4 bytes
	if (crDate != NULL)
	{
		memcpy(data+40, crDate->getAsBytes(), 4);
	}
	else
	{
		Date* tmp = new Date();
		memcpy(data+40, tmp->getAsBytes(), 4);
		delete tmp;
	}

	// last access date has 4 bytes
	if (laDate != NULL)
	{
		memcpy(data+44, laDate->getAsBytes(), 4);
	}
	else
	{
		Date* tmp = new Date();
		memcpy(data+44, tmp->getAsBytes(), 4);
		delete tmp;
	}

	// last update date has 4 bytes
	if (luDate != NULL)
	{
		memcpy(data+48, luDate->getAsBytes(), 4);
	}
	else
	{
		Date* tmp = new Date();
		memcpy(data+48, tmp->getAsBytes(), 4);
		delete tmp;
	}

	if (fileName != NULL)
	{
		memcpy(data+52, fileName, 128);
	}
	else
	{
		memset(data+52, 0, 128);
	}

	if (dirBlocks != NULL)
	{
		for (int i = 0; i < DIRECT_BLOCKADDR_ENTRIES; i++)
		{
			memcpy(data+180+(i*4), Utils::uLongToBytes(dirBlocks[i]), 4);
		}
	}
	else
	{
		memset(data+180, 0, 4*DIRECT_BLOCKADDR_ENTRIES);
	}

	if (indirBlocks != NULL)
	{
		for (int i = 0; i < (BLOCK_DIM-180-DIRECT_BLOCKADDR_ENTRIES*4)/4; i++)
		{
			memcpy(data+180+(4*DIRECT_BLOCKADDR_ENTRIES)+(i*4),
				Utils::uLongToBytes(indirBlocks[i]), 4);
		}
	}
	else
	{
		memset(data+180+(4*DIRECT_BLOCKADDR_ENTRIES), 0, BLOCK_DIM-180-DIRECT_BLOCKADDR_ENTRIES*4);
	}

	return data;
}
