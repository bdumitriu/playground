/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Fri Nov 8 2002
 * Description:		This is the implementation of the HDDFormatter class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/driver/HDDFormatter.h"

void HDDFormatter::formatHDD(HDD* hdd) throw(
		FileSizeTooLargeException*,
		NotEnoughSpaceException*,
		HardDiskNotInitializedException*,
		IOException*)
{
	// write the superblock
	Superblock* sb = new Superblock;

	unsigned char* checksum = new unsigned char[16];
	memcpy(checksum, "superblk chcksm\0", 16);
	sb->setChecksum(checksum);
	unsigned char* str = new unsigned char[12];
	memcpy(str, "bdumitriusfs", 12);
	sb->setSignature(str);
	sb->setNrOfBlocks(NR_OF_BLOCKS);
	sb->setBlockDimension(BLOCK_DIM);
	sb->setFirstInodeBlock(FILEATTR_AREA_START_ADDR);
	sb->setLastInodeBlock(FILEATTR_AREA_START_ADDR+MAX_NO_FILEATTR-1);
	sb->setRootDirInode(ROOT_DIR_FILEATTR);
	sb->setFreeBlocksInode(FREEBLOCKS_FILEATTR);

	sb->writeToHardDisk(hdd);
	if (str != NULL)
		delete [] str;
	if (sb != NULL)
		delete sb;

	// write the free inodes map
	InodeMap* map = new InodeMap();

	memcpy(checksum, "free ino chcksm\0", 16);
	map->setChecksum(checksum);
	map->setNrOfInodes(MAX_NO_FILEATTR);
	map->allocInode(ROOT_DIR_FILEATTR);
	map->allocInode(FREEBLOCKS_FILEATTR);
	map->allocInode(MAX_NO_FILEATTR-1);

	map->writeToHardDisk(hdd, FILEATTR_AREA_START_ADDR-1);
	if (map != NULL)
		delete map;

	// write the free blocks inode & file
	Inode* fbInode = new Inode();

	fbInode->setValid(true);
	fbInode->setFileType(NORMAL_FILE_TYPE);
	memcpy(checksum, "free blk chcksm\0", 16);
	fbInode->setChecksum(checksum);
	unsigned long reqBytes = nrOfRequiredBytesForFreeBlocksFile();
	fbInode->setFileSizeInBytes(reqBytes);
	fbInode->setFileSizeInBlocks(nrOfRequiredBlocksForFreeBlocksFile(reqBytes));
	fbInode->setFileNameSize(0);
	fbInode->setCreationDate(new Date());
	fbInode->setLastAccessDate(new Date());
	fbInode->setLastUpdateDate(new Date());

	// write the free blocks file
	writeBlocksMap(hdd, fbInode);

	// write the free blocks inode
	fbInode->writeToHardDisk(hdd, FILEATTR_AREA_START_ADDR+FREEBLOCKS_FILEATTR);

	if (checksum != NULL)
		delete [] checksum;
	if (fbInode != NULL)
		delete fbInode;
}

unsigned long HDDFormatter::nrOfRequiredBytesForFreeBlocksFile()
{
	// see how many non system blocks we have on the hard disk
	unsigned long blocks = 0;
	if ((NR_OF_BLOCKS-FILEATTR_AREA_START_ADDR-MAX_NO_FILEATTR) > 0)
		blocks = NR_OF_BLOCKS-FILEATTR_AREA_START_ADDR-MAX_NO_FILEATTR;

	/* Compute the number of required bytes in order to store the bitmap. */
	/* We need 1 bit / each non system block + 16 bytes (checksum) + 4 bytes */
	/* (to hold the nr. of non system blocks which represents the actual number */
	/* of used bits) */

	// compute the number of required bytes to store 1 bit / block
	unsigned long reqBytes;
	if ((blocks % 8) == 0)
	{
		reqBytes = (int) blocks/8;
	}
	else
	{
		reqBytes = ((int) (blocks/8))+1;
	}

	// add 20 bytes = 16 bytes (checksum) + 4 bytes (nr. of blocks)
	reqBytes += 20;

	return reqBytes;
}

unsigned long HDDFormatter::nrOfRequiredBlocksForFreeBlocksFile(unsigned long nrBytes)
{
	// compute the number of blocks to hold the required number of bytes
	unsigned long reqBlocks;
	if ((nrBytes % BLOCK_DIM) == 0)
	{
		reqBlocks = (int) nrBytes/BLOCK_DIM;
	}
	else
	{
		reqBlocks = ((int) (nrBytes/BLOCK_DIM))+1;
	}

	return reqBlocks;
}

unsigned long HDDFormatter::writeBlocksMap(HDD* hdd, Inode* fbInode)
	throw(FileSizeTooLargeException*, NotEnoughSpaceException*)
{
	unsigned long reqBytes = nrOfRequiredBytesForFreeBlocksFile();
	unsigned long reqBlocks = nrOfRequiredBlocksForFreeBlocksFile(reqBytes);

	// see how many more extra blocks required for indirect addressing.
	// we use BLOCK_DIM/4 because we need 4 bytes to store a block address,
	// therefore a block of addresses of blocks can hold BLOCK_DIM/4 addresses.
	unsigned long extraBlocks = 0;
	if (reqBlocks > DIRECT_BLOCKADDR_ENTRIES)
	{
		if (((reqBlocks-DIRECT_BLOCKADDR_ENTRIES) % (BLOCK_DIM/4)) == 0)
		{
			extraBlocks = (reqBlocks-DIRECT_BLOCKADDR_ENTRIES)/(BLOCK_DIM/4);
		}
		else
		{
			extraBlocks = ((int) (reqBlocks-DIRECT_BLOCKADDR_ENTRIES)/(BLOCK_DIM/4))+1;
		}
	}

	// see if the inode can accomodate that many indirect block addresses
	if (((BLOCK_DIM-180-DIRECT_BLOCKADDR_ENTRIES*4)/4) < extraBlocks)
	{
		throw new FileSizeTooLargeException();
	}

	/*
	 * What we do here is write a number of reqBlocks+extraBlocks blocks on disk.
	 * The first extraBlocks blocks on the disk will be written with the blocks
	 * used for indirect addressing, and the following reqBlocks blocks will be
	 * written with useful information. This means that the first extraBlocks+
	 * reqBlocks bits will be set to 1 (blocks are occupied) and the rest are
	 * set to 0.
	 * Also, the inode's values have to be set correctly.
	 */

	// compute the absolute index of the first block containing useful data
	// (i.e. skip the space before the inodes start, skip the inodes and skip
	// another extraBlocks number of blocks (reserved for blocks used for indiret
	// addressing))
	unsigned long stBlockAddr = FILEATTR_AREA_START_ADDR+MAX_NO_FILEATTR+extraBlocks;

	// temporary buffer
	unsigned char* data = new unsigned char[BLOCK_DIM];
	if (data == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}

	// the number of bits which have to be set to 1
	unsigned long totBits = extraBlocks+reqBlocks;

	// the first 20 bytes are special, since they contain the checksum
	// and the number of blocks	
	memset(data, 0, 16);
	if (NR_OF_BLOCKS-FILEATTR_AREA_START_ADDR-MAX_NO_FILEATTR >= 0)
		memcpy(data+16, Utils::uLongToBytes(NR_OF_BLOCKS-FILEATTR_AREA_START_ADDR-MAX_NO_FILEATTR), 4);
	else
		memset(data+16, 0, 4);
	
	if (totBits <= (BLOCK_DIM-20)*8)
	{
		memset(data+20, 0xff, (int) totBits/8);
		int tmp = 0;
		if ((totBits % 8) != 0)
		{
			tmp = 1;
			unsigned char c = 0;
			unsigned char mask = 0x80;
			for (unsigned int i = 0; i < (totBits % 8); i++)
			{
				c |= mask;
				mask >>= 1;
			}
			memcpy(data+20+((int) totBits/8), &c, 1);
		}
		memset(data+20+((int) totBits/8)+tmp, 0, BLOCK_DIM-20-((int) totBits/8)-tmp);
		totBits = 0;
	}
	else
	{
		memset(data+20, 0xff, BLOCK_DIM-20);
		totBits -= ((BLOCK_DIM-20)*8);
	}

	// write the first block
	try
	{
		hdd->writeHDDBlock(stBlockAddr, data);
	}
	catch (InvalidBlockNumberException* e)
	{
		throw new NotEnoughSpaceException("Hard disk does not contain enough space \
			to accomodate the free blocks file");
	}

	// write the rest of the blocks
	for (unsigned int i = 1; i < reqBlocks; i++)
	{
		if (totBits == 0)
		{
			memset(data, 0, BLOCK_DIM);
		}
		else if (totBits <= BLOCK_DIM*8)
		{
			memset(data, 0xff, (int) totBits/8);
			int tmp = 0;
			if ((totBits % 8) != 0)
			{
				tmp = 1;
				unsigned char c = 0;
				unsigned char mask = 0x80;
				for (unsigned int j = 0; j < (totBits % 8); j++)
				{
					c |= mask;
					mask >>= 1;
				}
				memcpy(data+((int) totBits/8), &c, 1);
			}
			memset(data+((int) totBits/8)+tmp, 0, BLOCK_DIM-((int) totBits/8)-tmp);
			totBits = 0;
		}
		else
		{
			memset(data, 0xff, BLOCK_DIM);
			totBits -= BLOCK_DIM*8;
		}

		hdd->writeHDDBlock(stBlockAddr+i, data);
	}

	// next we have to write the blocks used for indirect addressing with the
	// addresses of the indirectly addressed blocks (all previously written blocks
	// starting with the DIRECT_BLOCKADDR_ENTRIESth block)

	// what we need first is the absolute index of the first block used for indirect
	// addressing
	unsigned long stExtraBlockAddr = FILEATTR_AREA_START_ADDR+MAX_NO_FILEATTR;

	// make stBlockAddr point to the first block which is accessed indirectly
	stBlockAddr += DIRECT_BLOCKADDR_ENTRIES;

	for (unsigned int i = 0; i < extraBlocks; i++)
	{
		for (int j = 0; j < BLOCK_DIM/4; j++)
		{
			memcpy(data+j*4, Utils::uLongToBytes(stBlockAddr+i*(BLOCK_DIM/4)+j), 4);
		}
		hdd->writeHDDBlock(stExtraBlockAddr+i, data);
	}

	// reset stBlockAddr to its initial value
	stBlockAddr -= DIRECT_BLOCKADDR_ENTRIES;

	// modifiy the inode's addressing values
	for (int i = 0; i < DIRECT_BLOCKADDR_ENTRIES; i++)
	{
		fbInode->setDirectBlockAddress(i, stBlockAddr+i);
	}

	for (unsigned int i = 0; i < extraBlocks; i++)
	{
		fbInode->setIndirectBlockAddress(i, stExtraBlockAddr+i);
	}

	return reqBlocks+extraBlocks;
}
