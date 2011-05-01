/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Sun Nov 10 2002
 * Description:		This is the implementation of the FileOutputStream class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/driver/FileOutputStream.h"

FileOutputStream::FileOutputStream(HDD* hdd, Inode* inode, unsigned long inodeBlock, BlockMap* map)
{
	this->hdd = hdd;
	this->inode = inode;
	this->inodeBlock = inodeBlock;
	this->map = map;
	fp = 0;
	buffer = new unsigned char[BLOCK_DIM];
	if (buffer == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
}

FileOutputStream::~FileOutputStream()
{
	if (buffer != NULL)
		delete [] buffer;
}

void FileOutputStream::write(unsigned char* buf, unsigned long length) throw(
		NotEnoughSpaceException*,
		FileSizeTooLargeException*,
		IOException*,
		InvalidBlockNumberException*,
		HardDiskNotInitializedException*,
		ArrayIndexOutOfBoundsException*)
{
	// if file size would become larger than ULONG_MAX...
	if ((ULONG_MAX-fp) < length)
	{
		strstream str;
		str << "You wanted to write more than " << ULONG_MAX <<
			" bytes, which is not allowed." << ends;
		throw new FileSizeTooLargeException(str.str());
	}

	// the initial file size
	unsigned long fSize = inode->getFileSizeInBytes();

	// the number of bytes we still have available in
	// the last already allocated block of the file
	unsigned long availableBytes = (BLOCK_DIM - (fSize % BLOCK_DIM)) % BLOCK_DIM;

	// how many more blocks do we need?
	unsigned long dataBlocks = 0;
	unsigned long extraBlocks = 0;
	if ((fp + length) > (fSize + availableBytes))
	{
		// first see how many actual blocks we still need
		dataBlocks = (unsigned long) ((fp + length - fSize - availableBytes)/BLOCK_DIM);
		if (((fp + length - fSize - availableBytes) % BLOCK_DIM) != 0)
			dataBlocks++;

		// and now see how many extra blocks we need for indirection (if any)
		unsigned long ndadb = dataBlocks;	/* non directly addressed data blocks */
		if (inode->getFileSizeInBlocks()+dataBlocks <= DIRECT_BLOCKADDR_ENTRIES)
		{
			ndadb = 0;
			extraBlocks = 0;
		}
		else if (inode->getFileSizeInBlocks() <= DIRECT_BLOCKADDR_ENTRIES)
		{
			ndadb -= (DIRECT_BLOCKADDR_ENTRIES-inode->getFileSizeInBlocks());
			extraBlocks = (unsigned long) (ndadb/(BLOCK_DIM/4));
			if (ndadb % (BLOCK_DIM/4) != 0)
				extraBlocks++;
		}
		else
		{
			unsigned long fa;	/* free address locations in already alloc'd indirect block */
			fa = (BLOCK_DIM/4) -
				((inode->getFileSizeInBlocks() - DIRECT_BLOCKADDR_ENTRIES) % (BLOCK_DIM/4));
			if (fa == 128)
			{
				fa = 0;
			}

			if (ndadb <= fa)
			{
				extraBlocks = 0;
			}
			else
			{
				extraBlocks = (unsigned long) ((ndadb-fa)/(BLOCK_DIM/4));
				if ((ndadb-fa) % (BLOCK_DIM/4) != 0)
					extraBlocks++;
			}
		}
	}

	// do we have that many blocks available?
	unsigned long* blocks = new unsigned long[dataBlocks+extraBlocks];
	if (blocks == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	if (map->getFreeBlocks(blocks, dataBlocks+extraBlocks) != (dataBlocks+extraBlocks))
	{
		throw new NotEnoughSpaceException();
	}

	// if we got here, then in blocks we have addresses of
	// <dataBlocks>+<extraBlocks> free blocks...

	// do we have enough space left in the inode to accomodate the
	// addresses of the extra blocks?
	if (MAX_FILESIZE < fp+length)
	{
		throw new FileSizeTooLargeException();
	}

	// if we have enough space for everything, start by
	// allocating all the blocks we need and modifying
	// the inode's data
	createExtraSpace(blocks, dataBlocks, extraBlocks);
	inode->setFileSizeInBlocks(inode->getFileSizeInBlocks()+dataBlocks);
	if ((fp + length) > fSize)
	{
		inode->setFileSizeInBytes(fp+length);
	}

	inode->writeToHardDisk(hdd, inodeBlock);

	// now that we have all the space we need allocated
	// we can actually start writing (thank god...)
	unsigned long i = 0;
	while (length > 0)
	{
		unsigned long bytesWritten = writeToSingleBlock(buf+i, length);
		i += bytesWritten;
		length -= bytesWritten;
	}
}

void FileOutputStream::skip(unsigned long n)
{
	if ((ULONG_MAX-fp) <= n)
		fp = ULONG_MAX;
	else
		fp += n;
}

void FileOutputStream::setPointer(unsigned long n)
{
	fp = n;
}

unsigned long FileOutputStream::getPointer()
{
	return fp;
}

void FileOutputStream::createExtraSpace(unsigned long* blocks, unsigned long dataBlocks, unsigned long extraBlocks)
{
	// put the current file size in blocks in a variable
	unsigned long fBlocks = inode->getFileSizeInBlocks();

	// allocate all blocks
	map->allocBlocks(blocks, dataBlocks+extraBlocks);

	// as what we have in blocks is relative block numbers and
	// what we need in the inode are absoulte block numbers we need
	// to alter it a bit...
	for (unsigned long i = 0; i < dataBlocks+extraBlocks; i++)
		blocks[i] += FILEATTR_AREA_START_ADDR+MAX_NO_FILEATTR;

	// create a FileBlocks object to help with addressing...
	FileBlocks* fBlk = new FileBlocks(hdd, inode);

	// make fBlocks the number of indirectly addressed blocks
	// and write extra indirection block addresses in inode if
	// necessary
	if (fBlocks <= DIRECT_BLOCKADDR_ENTRIES)
		fBlocks = 0;
	else
	{
		fBlocks -= DIRECT_BLOCKADDR_ENTRIES;

		// find out the index of the last valid indirect block address
		// in the inode structure
		unsigned long lastValid = (unsigned long) (fBlocks/(BLOCK_DIM/4));

		if (fBlocks % (BLOCK_DIM/4) == 0)
			lastValid--;

		for (unsigned long i = lastValid + 1; i < lastValid + 1 + extraBlocks; i++)
		{
			inode->setIndirectBlockAddress(i, blocks[i-lastValid-1]);
		}
	}

	fBlk->setBlocks(inode->getFileSizeInBlocks(), dataBlocks, blocks+extraBlocks);

	if (fBlk != NULL)
		delete fBlk;
}

unsigned long FileOutputStream::writeToSingleBlock(unsigned char* buf, unsigned long length) throw(
	IOException*,
	InvalidBlockNumberException*,
	HardDiskNotInitializedException*,
	ArrayIndexOutOfBoundsException*)
{
	// see in which of the file's blocks (directly or indirectly addressed)
	// the bytes we want to write reside
	unsigned long blockIdx = fp/BLOCK_DIM;

	// create a FileBlocks object to help with addressing...
	FileBlocks* fBlk = new FileBlocks(hdd, inode);

	// the actual hard disk block in which the bytes we need to write are
	unsigned long blkNo = fBlk->getBlock(blockIdx);

	// see where we have to start writing
	unsigned long idx = fp % BLOCK_DIM;

	// see how many bytes we have to write
	unsigned long noBytes = length;
	if (noBytes > (BLOCK_DIM-idx))
		noBytes = BLOCK_DIM-idx;
	if (noBytes > (inode->getFileSizeInBytes()-fp))
		noBytes = inode->getFileSizeInBytes()-fp;

	if (noBytes < BLOCK_DIM)
	{
		// read the bytes currently in the block blkNo
		hdd->readHDDBlock(blkNo, buffer);
	}

	// write the new bytes
	memcpy(buffer+idx, buf, noBytes);

	// write the block on the disk
	hdd->writeHDDBlock(blkNo, buffer);

	// update the file pointer
	fp += noBytes;

	return noBytes;
}
