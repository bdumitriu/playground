/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Mon Nov 4 2002
 * Description:		This is the interface of the Superblock class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _SUPERBLOCK_H
#define _SUPERBLOCK_H

#include <stdlib.h>
#include <string.h>
#include "../defs.h"
#include "HDD.h"
#include "../utils/Utils.h"
#include "../exceptions/HardDiskNotInitializedException.h"
#include "../exceptions/InvalidBlockNumberException.h"
#include "../exceptions/IOException.h"

/**
 * This class represents an interface to the hard disk's superblock.
 * Thus it allows read/write access to the various superblock elements.
 * Here's the configuration of the superblock which this class assumes:
 *
 * <table border="1" align="center">
 * <caption>Superblock configuration</caption>
 * <tr>
 * <td align="center" bgcolor="cyan">16 bytes</td>
 * <td align="center" bgcolor="cyan">12 bytes</td>
 * <td align="center" bgcolor="cyan">4 bytes</td>
 * <td align="center" bgcolor="cyan">4 bytes</td>
 * <td align="center" bgcolor="cyan">4 bytes</td>
 * <td align="center" bgcolor="cyan">4 bytes</td>
 * <td align="center" bgcolor="cyan">4 bytes</td>
 * <td align="center" bgcolor="cyan">4 bytes</td>
 * <td align="center" bgcolor="cyan">rest up to include/defs.h::BLOCK_DIM</td>
 * </tr>
 * <tr>
 * <td align="center" bgcolor="lightblue">checksum</td>
 * <td align="center" bgcolor="lightblue">signature</td>
 * <td align="center" bgcolor="lightblue">number of blocks</td>
 * <td align="center" bgcolor="lightblue">block dimesion</td>
 * <td align="center" bgcolor="lightblue">first inode block</td>
 * <td align="center" bgcolor="lightblue">last inode block</td>
 * <td align="center" bgcolor="lightblue">root dir inode</td>
 * <td align="center" bgcolor="lightblue">free blocks inode</td>
 * <td align="center" bgcolor="lightblue">reserved</td>
 * </tr>
 * </table>
 *
 * Currently, the superblock signature is "bdumitriusfs" (without the ").
 *
 * @short Represents an interface to the hard disk's superblock.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */

/*
 * The superblock configuration is:
 *
 * +----------+-----------+---------------+------------+------------------->
 * | 16 bytes | 12 bytes  | 4 bytes       | 4 bytes    | 4 bytes           |
 * +----------+-----------+---------------+------------+------------------->
 * | checksum | signature | nr. of blocks | block dim. | first inode block |
 * +----------+-----------+---------------+------------+------------------->
 *
 * <------------------+----------------+------------------->
 * | 4 bytes          | 4 bytes        | 4 bytes           |
 * <------------------+----------------+------------------->
 * | last inode block | root dir inode | free blocks inode |
 * <------------------+----------------+------------------->
 *
 * <--------------------------------------+
 * | rest up to include/defs.h::BLOCK_DIM |
 * <--------------------------------------+
 * | reserved                             |
 * <--------------------------------------+
 *
 */
class Superblock
{
public:
	/**
	 * Creates a new Superblock and initializes all its members
	 * with 0 or arrays of 0.
	 */
	Superblock();

	/**
	 * The destructor of the class.
	 */
	~Superblock();

	/**
	 * Returns the value of the checksum data member.
	 *
 	 * @return the value of the checksum data member.
	 */
	unsigned char* getChecksum();
	
	/**
	 * Returns the value of the signature data member.
	 *
	 * @return the value of the signature data member.
	 */
	unsigned char* getSignature();

	/**
	 * Returns the value of the nrOfBlocks data member.
	 *
	 * @return the value of the nrOfBlocks data member.
	 */
	unsigned long getNrOfBlocks();

	/**
	 * Returns the value of the blockDimension data member.
	 *
	 * @return the value of the blockDimension data member.
	 */
	unsigned long getBlockDimension();
	
	/**
	 * Returns the value of the firstInodBlock data member.
	 *
	 * @return the value of the firstInodBlock data member.
	 */
	unsigned long getFirstInodeBlock();
	
	/**
	 * Returns the value of the lastInodeBlock data member.
	 *
	 * @return the value of the lastInodeBlock data member.
	 */
	unsigned long getLastInodeBlock();
	
	/**
	 * Returns the value of the rootDirInode data member.
	 *
	 * @return the value of the rootDirInode data member.
	 */
	unsigned long getRootDirInode();
	
	/**
	 * Returns the value of the freeBlocksInode data member.
	 *
	 * @return the value of the freeBlocksInode data member.
	 */
	unsigned long getFreeBlocksInode();

	/**
	 * Sets the checksum data member to <code>checksum</code>.
	 *
	 * @param checksum the new value of the checksum data member.
	 */
	void setChecksum(const unsigned char* checksum);

	/**
	 * Sets the signature data member to <code>signature</code>.
	 *
	 * @param signature the new value of the signature data member.
	 */
	void setSignature(const unsigned char* signature);

	/**
	 * Sets the nrOfBlocks data member to <code>nrOfBlocks</code>.
	 *
	 * @param nrOfBlocks the new value of the nrOfBlocks data member.
	 */
	void setNrOfBlocks(unsigned long nrOfBlocks);

	/**
	 * Sets the blockDimension data member to <code>blockDimension</code>.
	 *
	 * @param blockDimension the new value of the blockDimension data member.
	 */
	void setBlockDimension(unsigned long blockDimension);

	/**
	 * Sets the firstInodeBlock data member to <code>firstInodeBlock</code>.
	 *
	 * @param firstInodeBlock the new value of the firstInodeBlock data member.
	 */
	void setFirstInodeBlock(unsigned long firstInodeBlock);

	/**
	 * Sets the lastInodeBlock data member to <code>lastInodeBlock</code>.
	 *
	 * @param lastInodeBlock the new value of the lastInodeBlock data member.
	 */
	void setLastInodeBlock(unsigned long lastInodeBlock);

	/**
	 * Sets the rootDirInode data member to <code>rootDirInode</code>.
	 *
	 * @param rootDirInode the new value of the rootDirInode data member.
	 */
	void setRootDirInode(unsigned long rootDirInode);

	/**
	 * Sets the freeBlocksInode data member to <code>freeBlocksInode</code>.
	 *
	 * @param freeBlocksInode the new value of the freeBlocksInode data member.
	 */
	void setFreeBlocksInode(unsigned long freeBlocksInode);

	/**
	 * This method reads the superblock (block nr. 0) from the
	 * <code>hdd</code> hard disk and initializes all of the class'
	 * data members according to the superblock configuration scheme
	 * presented above.
	 *
	 * The method throws:
	 * <ul>
	 * <li> HardDiskNotInitializedException* forwarded from <code>hdd</code>.</li>
	 * <li> IOException* forwarded from <code>hdd</code>.</li>
	 * </ul>
	 *
	 * @param hdd the hard disk whose superblock to read.
	 */
	void readFromHardDisk(HDD* hdd) throw(HardDiskNotInitializedException*, IOException*);
	
	/**
	 * This method writes the superblock (block nr. 0) of the
	 * <code>hdd</code> hard disk with its data member values
	 * according to the superblock configuration scheme presented
	 * above.
	 *
	 * If any of the array data members is NULL, zero (0) characters will
	 * be written for the entire length of the array.
	 *
	 * The method throws:
	 * <ul>
	 * <li> HardDiskNotInitializedException* forwarded from <code>hdd</code>.</li>
	 * <li> IOException* forwarded from <code>hdd</code>.</li>
	 * </ul>
	 *
	 * @param hdd the hard disk whose superblock to write.
	 */
	void writeToHardDisk(HDD* hdd)
 		throw(HardDiskNotInitializedException*, IOException*);

	/**
	 * This method returns the superblock in the exact same manner as it is
	 * stored on the hard disk (as an array of include/defs.h::BLOCK_DIM
	 * unsigned chars).
	 *
	 * If any of the array data members is NULL, zero (0) characters will
	 * be written for the entire length of the array.
	 *
	 * @return the superblock as it is stored on the hard disk.
	 */
	unsigned char* getAsBytes();

private:
	unsigned char* checksum;	/* the checksum */
	unsigned char* signature;	/* the signature */
	unsigned long nrBlocks;		/* the total number of blocks */
	unsigned long blockDim;		/* the dimension of a block in bytes */
	unsigned long inodeStart;	/* the zero-based index of the first inode on disk */
	unsigned long inodeEnd;		/* the zero-based index of the last inode on disk */
	unsigned long roodDir;		/* the zero-based index of the inode conatining the root directory */
	unsigned long freeBlocks;	/* the zero-based index of the inode conatining the free blocks file */

	/**
	 * This method returns a serialized representation of the superblock (as it's
	 * supposed to appear on the hard disk).
	 *
	 * If any of the array data members is NULL, zero (0) characters will
	 * be written for the entire length of the array.
	 *
	 * @return a include/defs.h::BLOCK_DIM byte representation of the superblock.
	 */
	unsigned char* serialize();
};

#endif
