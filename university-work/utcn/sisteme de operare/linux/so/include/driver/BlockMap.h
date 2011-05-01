/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Sat Nov 9 2002
 * Description:		This is the interface of the BlockMap class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _BLOCK_MAP_H
#define _BLOCK_MAP_H

#include <stdlib.h>
#include <string.h>
#include <iostream.h>
#include "../defs.h"
#include "HDD.h"
#include "Inode.h"
#include "FileInputStream.h"
#include "FileOutputStream.h"
#include "../utils/Utils.h"
#include "../exceptions/ArrayIndexOutOfBoundsException.h"
#include "../exceptions/HardDiskNotInitializedException.h"
#include "../exceptions/InvalidBlockNumberException.h"
#include "../exceptions/IOException.h"

/**
 * This class represents an interface to the structures on the hard
 * disk containing the blocks bitmap. It allows easy manipulation of
 * this bitmap (finding free blocks, allocating blocks, freeing blocks,
 * etc.). Here's the configuration of the blocks bitmap file which
 * this class assumes:
 *
 * <table border="1" align="center">
 * <caption>Blocks bitmap file configuration</caption>
 * <tr>
 * <td align="center" bgcolor="cyan">16 bytes</td>
 * <td align="center" bgcolor="cyan">4 bytes</td>
 * <td align="center" bgcolor="cyan">nr. of non system blocks <u>bits</u></td>
 * </tr>
 * <tr>
 * <td align="center" bgcolor="lightblue">checksum</td>
 * <td align="center" bgcolor="lightblue">total number of non system blocks on hard disk</td>
 * <td align="center" bgcolor="lightblue">blocks bitmap</td>
 * </tr>
 * </table>
 *
 * The blocks bitmap contains, say, n bits. Then the bitmap holds
 * information regarding a exactly n blocks (one bit/block). If the bit
 * is 1, then the block is allocated (i.e. is used by a file/directory);
 * if the bit is 0, then the block is free for use. The actual number of
 * bits which represent blocks is given by the second element in the block
 * (the number of non system blocks).
 *
 * @short Represents an interface to the hard disk's structure containg the blocks bitmap.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */

/*
 * The blocks bitmap file configuration is:
 *
 * +----------+--------------------------+--------------------------------------+
 * | 16 bytes | 4 bytes                  | nr. of non system blocks * 8 *bits*  |
 * +----------+--------------------------+--------------------------------------+
 * | checksum | nr. of non system blocks | the blocks bitmap                    |
 * +----------+--------------------------+--------------------------------------+
 *
 */
class BlockMap
{
public:
	/**
	 * Creates a new BlockMap. <code>hdd</code> will be used to perform
	 * read/write operations when the class' methods are called.
	 *
	 * @param hdd the hard disk which will be used for read/write operations.
	 */
	BlockMap(HDD* hdd);

	/**
	 * The destructor of the class. This destructor does not free
	 * the <code>hdd</code>.
	 */
	~BlockMap();

	/**
	 * Returns the value of the blocks bitmap file's checksum.
	 *
	 * The method throws:
	 * <ul>
	 * <li> HardDiskNotInitializedException* forwarded from <code>hdd</code>.</li>
	 * <li> IOException* forwarded from <code>hdd</code>.</li>
	 * <li> InvalidBlockNumberException* forwarded from <code>hdd</code>.</li>
	 * </ul>
	 *
	 * @return the value of the blocks bitmap file's checksum.
	 */
	unsigned char* getChecksum()
		throw(HardDiskNotInitializedException*, IOException*, InvalidBlockNumberException*);

	/**
	 * Returns the value of the blocks bitmap file's number of non system blocks.
	 *
	 * The method throws:
	 * <ul>
	 * <li> HardDiskNotInitializedException* forwarded from <code>hdd</code>.</li>
	 * <li> IOException* forwarded from <code>hdd</code>.</li>
	 * <li> InvalidBlockNumberException* forwarded from <code>hdd</code>.</li>
	 * </ul>
	 *
	 * @return the value of the blocks bitmap file's number of non system blocks.
	 */
	unsigned long getNrOfBlocks()
		throw(HardDiskNotInitializedException*, IOException*, InvalidBlockNumberException*);

	/**
	 * Changes the value of the blocks bitmap file's checksum to
	 * <code>checksum</code>.
	 *
	 * The method throws:
	 * <ul>
	 * <li> HardDiskNotInitializedException* forwarded from <code>hdd</code>.</li>
	 * <li> IOException* forwarded from <code>hdd</code>.</li>
	 * <li> InvalidBlockNumberException* forwarded from <code>hdd</code>.</li>
	 * </ul>
	 *
	 * @param checksum the new value of the checksum.
	 */
	void setChecksum(unsigned char* checksum)
		throw(HardDiskNotInitializedException*, IOException*, InvalidBlockNumberException*);

	/**
	 * Changes the value of the blocks bitmap file's number of non
	 * system blocks to <code>nrOfBlocks</code>.
	 *
	 * The method throws:
	 * <ul>
	 * <li> HardDiskNotInitializedException* forwarded from <code>hdd</code>.</li>
	 * <li> IOException* forwarded from <code>hdd</code>.</li>
	 * <li> InvalidBlockNumberException* forwarded from <code>hdd</code>.</li>
	 * </ul>
	 *
	 * @param nrOfBlocks the new value of the number of non system blocks.
	 */
	void setNrOfBlocks(unsigned long nrOfBlocks)
		throw(HardDiskNotInitializedException*, IOException*, InvalidBlockNumberException*);

	/**
	 * Returns true if the <code>index</code>th bit of the bitmap
	 * is 0 and false if it is 1.
	 *
	 * The method throws:
	 * <ul>
	 * <li> ArrayIndexOutOfBoundsException* if index is less than 0 (which
	 * is rather hard since it's an unsigned long) or greater than or
	 * equal to NR_OF_BLOCKS-FILEATTR_AREA_START_ADDR-MAX_NO_FILEATTR.</li>
	 * <li> HardDiskNotInitializedException* forwarded from <code>hdd</code>.</li>
	 * <li> IOException* forwarded from <code>hdd</code>.</li>
	 * <li> InvalidBlockNumberException* forwarded from <code>hdd</code>.</li>
	 * </ul>
	 *
	 * @return true if the <code>index</code>th bit is 0 and false
	 * if it is 1.
	 */
	bool isFree(unsigned long index) throw(
		ArrayIndexOutOfBoundsException*,
		HardDiskNotInitializedException*,
		IOException*,
		InvalidBlockNumberException*);

	/**
	 * Sets the <code>index</code>th bit of the bitmap to 1.
	 *
	 * The method doesn't make any checks too see if the block isn't
	 * already allocated. You should make sure of that yourself.
	 *
	 * The method throws:
	 * <ul>
	 * <li> ArrayIndexOutOfBoundsException* if index is less than 0 (which
	 * is rather hard since it's an unsigned long) or greater than or
	 * equal to 8*include/defs.h::NR_OF_BLOCKS-FILEATTR_AREA_START_ADDR-MAX_NO_FILEATTR.</li>
	 * <li> HardDiskNotInitializedException* forwarded from <code>hdd</code>.</li>
	 * <li> IOException* forwarded from <code>hdd</code>.</li>
	 * <li> InvalidBlockNumberException* forwarded from <code>hdd</code>.</li>
	 * </ul>
	 */
	void allocBlock(unsigned long index) throw(
		ArrayIndexOutOfBoundsException*,
		HardDiskNotInitializedException*,
		IOException*,
		InvalidBlockNumberException*);

	/**
	 * Allocates each of the blocks in the <code>blocks</code> array by
	 * setting its corresponding bit to 1. <code>nr</code> represents the
	 * number of blocks to allocated. Therefore, blocks should contain at
	 * least <code>nr</code> valid entries.
	 *
	 * The method doesn't make any checks too see if the blocks aren't
	 * already allocated. You should make sure of that yourself.
	 *
	 * The method throws:
	 * <ul>
	 * <li> ArrayIndexOutOfBoundsException* if any of the blocks is less
	 * than 0 (which is rather hard since they're unsigned longs) or greater
	 * than or equal to 8*include/defs.h::NR_OF_BLOCKS-FILEATTR_AREA_START_ADDR-MAX_NO_FILEATTR.</li>
	 * <li> HardDiskNotInitializedException* forwarded from <code>hdd</code>.</li>
	 * <li> IOException* forwarded from <code>hdd</code>.</li>
	 * <li> InvalidBlockNumberException* forwarded from <code>hdd</code>.</li>
	 * </ul>
	 */
	void allocBlocks(unsigned long* blocks, unsigned long nr) throw(
		ArrayIndexOutOfBoundsException*,
		HardDiskNotInitializedException*,
		IOException*,
		InvalidBlockNumberException*);

	/**
	 * Sets the <code>index</code>th bit of the bitmap to 0.
	 *
	 * The method throws:
	 * <ul>
	 * <li> ArrayIndexOutOfBoundsException* if index is less than 0 (which
	 * is rather hard since it's an unsigned long) or greater than or
	 * equal to 8*include/defs.h::NR_OF_BLOCKS-FILEATTR_AREA_START_ADDR-MAX_NO_FILEATTR.</li>
	 * <li> HardDiskNotInitializedException* forwarded from <code>hdd</code>.</li>
	 * <li> IOException* forwarded from <code>hdd</code>.</li>
	 * <li> InvalidBlockNumberException* forwarded from <code>hdd</code>.</li>
	 * </ul>
	 */
	void freeBlock(unsigned long index) throw(
		ArrayIndexOutOfBoundsException*,
		HardDiskNotInitializedException*,
		IOException*,
		InvalidBlockNumberException*);

	/**
	 * Returns the index of a free bitmap position (i.e. one whose value
	 * is 0). This method takes the nr. of non system blocks into account,
	 * meaning it only searches for a free bit up to (this number-1)th bit.
	 * If no free positions exist, the method returns the same value
	 * getNrOfBlocks() would return => you can check if you still have free
	 * bytes or not by comparing the return value received with the return
	 * value of getNrOfBlocks(). If they are identical, it means the hard
	 * disk is full (i.e. no more free non system blocks are available).
	 *
	 * The method throws:
	 * <ul>
	 * <li> HardDiskNotInitializedException* forwarded from <code>hdd</code>.</li>
	 * <li> IOException* forwarded from <code>hdd</code>.</li>
	 * <li> InvalidBlockNumberException* forwarded from <code>hdd</code>.</li>
	 * </ul>
	 *
	 * @return the index of a free bitmap position (i.e. one whose value is 0).
	 */
	unsigned long getFreeBlock()
		throw(HardDiskNotInitializedException*, IOException*, InvalidBlockNumberException*);

	/**
	 * Searches for a number of <code>nr</code> free blocks and writes their
	 * addresses in the <code>freeBlocks</code> array. Therefore, <code>freeBlocks</code>
	 * should have enough space to store at least a number of <code>nr</code>
	 * unsigned longs. This method takes the nr. of non system blocks into account,
	 * meaning it only searches for free bits up to (this number-1)th bit. The
	 * method returns the number of free blocks found. This means that the first
	 * that many entries in freeBlocks are addresses of actual free blocks.
	 *
	 * The method throws:
	 * <ul>
	 * <li> HardDiskNotInitializedException* forwarded from <code>hdd</code>.</li>
	 * <li> IOException* forwarded from <code>hdd</code>.</li>
	 * <li> InvalidBlockNumberException* forwarded from <code>hdd</code>.</li>
	 * </ul>
	 *
	 * @return the the number of free blocks found.
	 */
	unsigned long getFreeBlocks(unsigned long* freeBlocks, unsigned long nr)
		throw(HardDiskNotInitializedException*, IOException*, InvalidBlockNumberException*);
	

private:
	HDD* hdd;			/* the hard disk used for read/write operations */
	FileInputStream* fis;		/* the file input stream used to read the file */
	FileOutputStream* fos;		/* the file output stream used to write the file */
	Inode* fbInode;			/* the inode holding information about the blocks bitmap file */
};

#endif
