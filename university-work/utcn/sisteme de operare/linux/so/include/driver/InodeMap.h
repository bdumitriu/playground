/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Thu Nov 7 2002
 * Description:		This is the interface of the InodeMap class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _INODE_MAP_H
#define _INODE_MAP_H

#include <stdlib.h>
#include <string.h>
#include <iostream.h>
#include "../defs.h"
#include "HDD.h"
#include "../utils/Utils.h"
#include "../exceptions/ArrayIndexOutOfBoundsException.h"
#include "../exceptions/HardDiskNotInitializedException.h"
#include "../exceptions/InvalidBlockNumberException.h"
#include "../exceptions/IOException.h"

/**
 * This class represents an interface to the block on the hard
 * disk containing the inodes bitmap. Thus it allows read/write
 * access to all elements in this block.
 * Here's the configuration of the block which this class assumes:
 *
 * <table border="1" align="center">
 * <caption>Inode map configuration</caption>
 * <tr>
 * <td align="center" bgcolor="cyan">16 bytes</td>
 * <td align="center" bgcolor="cyan">4 bytes</td>
 * <td align="center" bgcolor="cyan">rest up to include/defs.h::BLOCK_DIM</td>
 * </tr>
 * <tr>
 * <td align="center" bgcolor="lightblue">checksum</td>
 * <td align="center" bgcolor="lightblue">total number of inodes</td>
 * <td align="center" bgcolor="lightblue">inodes bitmap</td>
 * </tr>
 * </table>
 *
 * The inodes bitmap contains, say, n bytes. Then the bitmap can hold
 * information regarding a maxiumum of 8*n inodes (one bit/inode). If the bit
 * is 1, then the inode is allocated (i.e. the inode's valid bit is 1);
 * if the bit is 0, then the inode is free (i.e. the inode's valid bit is 0).
 * The actual number of bits which represent inodes is given by the second
 * element in the block (the total number of inodes). If the total number
 * of inodes is, say, 200 then 200/8 = 25 bytes are actually used.
 *
 * @short Represents an interface to the hard disk's block containg the inodes bitmap.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */

/*
 * The inodes bitmap block configuration is:
 *
 * +----------+---------------+--------------------------------------+
 * | 16 bytes | 4 bytes       | rest up to include/defs.h::BLOCK_DIM |
 * +----------+---------------+--------------------------------------+
 * | checksum | nr. of inodes | the inodes bitmap                    |
 * +----------+---------------+--------------------------------------+
 *
 */
class InodeMap
{
public:
	/**
	 * Creates a new InodeMap and initializes all its members with 0 or
	 * arrays of 0.
	 */
	InodeMap();

	/**
	 * The destructor of the class.
	 */
	~InodeMap();

	/**
	 * Returns the value of the checksum data member.
	 *
	 * @return the value of the checksum data member.
	 */
	unsigned char* getChecksum();

	/**
	 * Returns the value of the nrOfInodes data member.
	 *
	 * @return the value of the nrOfInodes data member.
	 */
	unsigned long getNrOfInodes();

	/**
	 * Sets the checksum data member to <code>checksum</code>.
	 *
	 * @param checksum the new value of the checksum data member.
	 */
	void setChecksum(unsigned char* checksum);

	/**
	 * Sets the nrOfInodes data member to <code>nrOfInodes</code>.
	 *
	 * @param nrOfInodes the new value of the nrOfInodes data member.
	 */
	void setNrOfInodes(unsigned long nrOfInodes);

	/**
	 * Returns true if the <code>index</code>th bit is 0 and false
	 * if it is 1.
	 *
	 * The method throws:
	 * <ul>
	 * <li> ArrayIndexOutOfBoundsException* if index is less than 0 (which
	 * is rather hard since it's an unsigned long) or greater than or
	 * equal to 8*include/defs.h::BLOCK_DIM-20.</li>
	 * </ul>
	 *
	 * @return true if the <code>index</code>th bit is 0 and false
	 * if it is 1.
	 */
	bool isFree(unsigned long index) throw(ArrayIndexOutOfBoundsException*);

	/**
	 * Sets the <code>index</code>th bit of the bitmap to 1.
	 *
	 * The method throws:
	 * <ul>
	 * <li> ArrayIndexOutOfBoundsException* if index is less than 0 (which
	 * is rather hard since it's an unsigned long) or greater than or
	 * equal to 8*include/defs.h::BLOCK_DIM-20.</li>
	 * </ul>
	 */
	void allocInode(unsigned long index) throw(ArrayIndexOutOfBoundsException*);

	/**
	 * Sets the <code>index</code>th bit of the bitmap to 0.
	 *
	 * The method throws:
	 * <ul>
	 * <li> ArrayIndexOutOfBoundsException* if index is less than 0 (which
	 * is rather hard since it's an unsigned long) or greater than or
	 * equal to 8*include/defs.h::BLOCK_DIM-20.</li>
	 * </ul>
	 */
	void freeInode(unsigned long index);

	/**
	 * Returns the index of a free bitmap position (i.e. one whose value
	 * is 0). This method takes the nrOfInodes data member into account,
	 * meaning it only searches for a free bit up to the (nrOfInodes-1)th bit.
	 * If no free positions exist, the method returns include/defs.h::BLOCK_DIM-20.
	 *
	 * @return the index of a free bitmap position (i.e. one whose value is 0).
	 */
	unsigned long getFreeBit();

	/**
	 * This method reads the inode map from block number <code>blockNo</code>
	 * of the <code>hdd</code> hard disk and initializes all of the class'
	 * data members according to the inode map configuration scheme
	 * presented above.
	 *
	 * The method throws:
	 * <ul>
	 * <li> HardDiskNotInitializedException* forwarded from <code>hdd</code>.</li>
	 * <li> IOException* forwarded from <code>hdd</code>.</li>
	 * <li> InvalidBlockNumberException* forwarded from <code>hdd</code>.</li>
	 * </ul>
	 *
	 * @param hdd the hard disk where to read the inode from.
	 * @param blockNo the number of the hard disk block to read.
	 */
	void readFromHardDisk(HDD* hdd, unsigned long blockNo)
		throw(HardDiskNotInitializedException*, IOException*, InvalidBlockNumberException*);
	
	/**
	 * This method writes the inode map as the <code>blockNo</code>th block
	 * of the <code>hdd</code> hard disk with its data member values
	 * according to the inode map configuration scheme presented
	 * above.
	 *
	 * If any of the array data members is NULL, zero (0) characters will
	 * be written for the entire length of the array.
	 *
	 * The method throws:
	 * <ul>
	 * <li> HardDiskNotInitializedException* forwarded from <code>hdd</code>.</li>
	 * <li> IOException* forwarded from <code>hdd</code>.</li>
	 * <li> InvalidBlockNumberException* forwarded from <code>hdd</code>.</li>
	 * </ul>
	 *
	 * @param hdd the hard disk to write the inode map to.
	 * @param blockNo the number of the hard disk block to write.
	 */
	void writeToHardDisk(HDD* hdd, unsigned long blockNo)
 		throw(HardDiskNotInitializedException*, IOException*, InvalidBlockNumberException*);

	/**
	 * This method returns the inode map in the exact same manner as it is
	 * stored on the hard disk (as an array of include/defs.h::BLOCK_DIM
	 * unsigned chars).
	 *
	 * If any of the array data members is NULL, zero (0) characters will
	 * be written for the entire length of the array.
	 *
	 * @return the inode map as it is stored on the hard disk.
	 */
	unsigned char* getAsBytes();

private:
	unsigned char* checksum;
	unsigned long nrInodes;
	unsigned char* bitmap;

	/**
	 * This method returns a serialized representation of the inode map
	 * (as it's supposed to appear on the hard disk).
	 *
	 * If any of the array data members is NULL, zero (0) characters will
	 * be written for the entire length of the array.
	 *
	 * @return a include/defs.h::BLOCK_DIM byte representation of the inode map.
	 */
	unsigned char* serialize();
};

#endif
