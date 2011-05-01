/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Sat Nov 9 2002
 * Description:		This is the implementation of the FileInputStream class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/driver/FileInputStream.h"

FileInputStream::FileInputStream(HDD* hdd, Inode* inode)
{
	this->hdd = hdd;
	this->inode = inode;
	fp = 0;
	buffer = new unsigned char[BLOCK_DIM];
	if (buffer == NULL)
	{
		cout << "Not enough memory.\n";	
		exit(1);
	}
	bufferValid = false;
	address  = new unsigned long[BLOCK_DIM/4];
	if (address == NULL)
	{
		cout << "Not enough memory.\n";	
		exit(1);
	}
	addressValid = false;
	buffering = false;
}

FileInputStream::~FileInputStream()
{
	if (buffer != NULL)
		delete [] buffer;
	if (address != NULL)
		delete [] buffer;
}

unsigned long FileInputStream::available()
{
	return inode->getFileSizeInBytes()-fp;
}

unsigned char FileInputStream::read() throw(
		IOException*,
		InvalidBlockNumberException*,
		HardDiskNotInitializedException*,
		ArrayIndexOutOfBoundsException*)
{
	unsigned char* byte = new unsigned char[1];
	if (byte == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	
	if (fp == inode->getFileSizeInBytes())
	{
		byte[0] = 0;
	}
	else
	{
		read(byte, 1);
	}

	return byte[0];
}

unsigned long FileInputStream::read(unsigned char* buf, unsigned long length) throw(
	IOException*,
	InvalidBlockNumberException*,
	HardDiskNotInitializedException*,
	ArrayIndexOutOfBoundsException*)
{
	if (length > (inode->getFileSizeInBytes()-fp))
		length = inode->getFileSizeInBytes()-fp;
	unsigned long retVal = length;

	unsigned long i = 0;
	while (length > 0)
	{
		unsigned long bytesRead = readFromSingleBlock(buf+i, length);
		i += bytesRead;
		length -= bytesRead;
	}

	return retVal;
}

unsigned long FileInputStream::skip(unsigned long n)
{
	unsigned long maxSkip = inode->getFileSizeInBytes()-fp;
	if (n <= maxSkip)
	{
		fp += n;
		return n;
	}
	else
	{
		fp = inode->getFileSizeInBytes();
		return inode->getFileSizeInBytes();
	}
}

unsigned long FileInputStream::setPointer(unsigned long n)
{
	if (n <= inode->getFileSizeInBytes())
	{
		fp = n;
	}
	else
	{
		fp = inode->getFileSizeInBytes();
	}

	return fp;
}

unsigned long FileInputStream::getPointer()
{
	return fp;
}

unsigned long FileInputStream::readFromSingleBlock(unsigned char* buf, unsigned long length) throw(
	IOException*,
	InvalidBlockNumberException*,
	HardDiskNotInitializedException*,
	ArrayIndexOutOfBoundsException*)
{
	// see in which of the file's blocks (directly or indirectly addressed)
	// the bytes we are looking for reside
	unsigned long blockIdx = fp/BLOCK_DIM;

	// the actual hard disk block in which the bytes we need to read are
	unsigned long blkNo;

	if (blockIdx < DIRECT_BLOCKADDR_ENTRIES)
	{
		blkNo = inode->getDirectBlockAddress(blockIdx);
	}
	else
	{
		// compute the index of the entry in the indirect block
		// adrresses table of the inode which contains the address
		// of the block containing the address of the block we are
		// looking for (indirect addressing makes explanations
		// difficult :-()
		unsigned long indirectBlockIdx =
			(blockIdx-DIRECT_BLOCKADDR_ENTRIES)/(BLOCK_DIM/4);

		// obtain the number of the physical block conating the
		// block address we need
		unsigned long indirectBlockNo = inode->getIndirectBlockAddress(indirectBlockIdx);

		// see if the block of addresses we have in our buffer isn't
		// by any chance the one we need and, if it isn't, modify it!
		if ((!buffering) || (!addressValid) || (addressValid && (addrNo != indirectBlockNo)))
		{
			unsigned char* data = new unsigned char[BLOCK_DIM];
			if (data == NULL)
			{
				cout << "Not enough memory.\n";
				exit(1);
			}

			hdd->readHDDBlock(indirectBlockNo, data);

			addressValid = true;
			for (unsigned long i = 0; i < BLOCK_DIM/4; i++)
				address[i] = Utils::bytesToULong(data+i*4);
			addrNo = indirectBlockNo;

			if (data != NULL)
				delete [] data;
		}

		// now that we have the addresses from the indirect addressing
		// block in our <address> structure, identify the address of the
		// block to read the actual data from
		blkNo = address[(blockIdx-DIRECT_BLOCKADDR_ENTRIES)%(BLOCK_DIM/4)];
	}

	// see if the block we have in our buffer isn't by any chance the
	// one we need and, if it isn't, modify it!
	if ((!buffering) || (!bufferValid) || (bufferValid && (blockNo != blkNo)))
	{
		bufferValid = true;
		hdd->readHDDBlock(blkNo, buffer);
		blockNo = blkNo;
	}

	// see where we have to start reading from
	unsigned long idx = fp % BLOCK_DIM;

	// see how many bytes we have to read
	unsigned long noBytes = length;
	if (noBytes > (BLOCK_DIM-idx))
		noBytes = BLOCK_DIM-idx;
	if (noBytes > (inode->getFileSizeInBytes()-fp))
		noBytes = inode->getFileSizeInBytes()-fp;

	// copy the bytes in the output buffer
	memcpy(buf, buffer+idx, noBytes);

	// update the file pointer
	fp += noBytes;

	return noBytes;
}

void FileInputStream::enableBuffering()
{
	buffering = true;
}

void FileInputStream::disableBuffering()
{
	buffering = false;
}

bool FileInputStream::bufferingEnabled()
{
	return buffering;
}

void FileInputStream::invalidateBuffer()
{
	bufferValid = false;
	addressValid = false;
}
