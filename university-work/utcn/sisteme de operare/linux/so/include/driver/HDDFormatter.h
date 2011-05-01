/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Fri Nov 8 2002
 * Description:		This is the interface of the HDDFormatter class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _HDD_FORMATTER_H
#define _HDD_FORMATTER_H

#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include "../defs.h"
#include "../utils/Utils.h"
#include "HDD.h"
#include "Superblock.h"
#include "Inode.h"
#include "InodeMap.h"
#include "../exceptions/NotEnoughSpaceException.h"
#include "../exceptions/FileSizeTooLargeException.h"
#include "../exceptions/HardDiskNotInitializedException.h"
#include "../exceptions/IOException.h"

/**
 * This class offers a simple, yet very useful, functionality, namely
 * if formats an already existing hard disk, writing the superblock, the
 * free inodes bitmap, the inodes (allocates 1 root dir inode & 1 inode
 * for the file containing the free blocks bitmap) and the free inodes
 * file.
 *
 * @short Represents an interface to any of the hard disk's inodes.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */
class HDDFormatter
{
public:

	/**
	 * Formats the <code>hdd</code> hard disk. It writes the superblock,
	 * the free inodes bitmap, the inode containing the free blocks bitmap
	 * file and the actual free blocks bitmap file.
	 *
	 * The method throws:
	 * <ul>
	 * <li> FileSizeTooLargeException* if inode structure does not contain enough
	 *	addressing space to accomodate the dimension of the free blocks bitmap
	 *	file.</li>
	 * <li> NotEnoughSpaceException* if the hard disk does not have enough space
	 *	to accomodate the free blocks bitmap file. This situation is possible
	 *	only when the hard disk has no non system block (i.e. it is formed
	 *	only of system blocks) and still we would need 1 block to write the
	 *	free blocks bitmap file (the file always requires at least 20 bytes
	 *	= 16 bytes for checksum + 4 bytes for the number of blocks).</li>
	 * <li> HardDiskNotInitializedException* forwarded from <code>hdd</code>.</li>
	 * <li> IOException* forwarded from <code>hdd</code>.</li>
	 * </ul>
	 *
	 */
	static void formatHDD(HDD* hdd) throw(
		FileSizeTooLargeException*,
		NotEnoughSpaceException*,
		HardDiskNotInitializedException*,
		IOException*);

private:
	/**
	 * This methods computes the number of blocks necesarry in order to
	 * accomodate the free blocks bitmap and initializes the appropriate hard
	 * disk blocks. It also completes the <code>freeBlocksInode</code>'s
	 * direct block addresses & indirect block addresses accordingly. It does
	 * not, however, write the inode to disk. This operation should be performed
	 * externally.
	 *
	 * The method throws:
	 * <ul>
	 * <li> FileSizeTooLargeException* if inode structure does not contain enough
	 *	addressing space to accomodate the dimension of the free blocks bitmap
	 *	file.</li>
	 * <li> NotEnoughSpaceException* if the hard disk does not have enough space
	 *	to accomodate the free blocks bitmap file. This situation is possible
	 *	only when the hard disk has no non system block (i.e. it is formed
	 *	only of system blocks) and still we would need 1 block to write the
	 *	free blocks bitmap file (the file always requires at least 20 bytes
	 *	= 16 bytes for checksum + 4 bytes for the number of blocks).</li>
	 * </ul>
	 *
	 * @return the total number of written blocks (including those used for
	 *	indirect addressing).
	 */
	static unsigned long writeBlocksMap(HDD* hdd, Inode* freeBlocksInode)
		throw(FileSizeTooLargeException*, NotEnoughSpaceException*);

	/**
	 * Returns the number of bytes required to hold the free blocks
	 * file (1 bit / non system block + 16 bytes (checksum) + 4 bytes
	 * (nr. of non system blocks)).
	 *
	 * @return the number of bytes required to hold the free blocks
	 *	file.
	 */
	static unsigned long nrOfRequiredBytesForFreeBlocksFile();

	/**
	 * Returns the actual number of blocks required to hold <code>nrBytes</code>
	 * bytes (not including the extra blocks required for indirect addressing).
	 *
	 * @param nrBytes the number of bytes which need to be stored in blocks.
	 * @return the actual number of blocks required to hold <code>nrBytes</code>
	 *	bytes.
	 */
	static unsigned long nrOfRequiredBlocksForFreeBlocksFile(unsigned long nrBytes);
};

#endif
