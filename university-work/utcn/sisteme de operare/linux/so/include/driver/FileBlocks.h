/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Sun Nov 10 2002
 * Description:		This is the interface of the FileBlocks class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _FILE_BLOCKS_H
#define _FILE_BLOCKS_H

#include <iostream.h>
#include <string.h>
#include <stdlib.h>
#include "../defs.h"
#include "../utils/Utils.h"
#include "HDD.h"
#include "Inode.h"
#include "../exceptions/ArrayIndexOutOfBoundsException.h"
#include "../exceptions/HardDiskNotInitializedException.h"
#include "../exceptions/InvalidBlockNumberException.h"
#include "../exceptions/IOException.h"

/**
 * This class makes inode addressing transparent. It allows the
 * user not to care whether (s)he's working with direct or indirect
 * addressing.
 *
 * @short Provides a simple means to use the blocks which contain a file's data.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */
class FileBlocks
{
public:
	/**
	 * Creates a new FileBlocks. <code>hdd</code> is the hard disk to be
	 * used for read/write operations (what will be read/written are the blocks
	 * used for indirect addressing). <code>inode</code> represents the
	 * inode associated with the file whose block addresses you want to
	 * read/write. The <code>inode</code> received as parameter is supposed
	 * to be valid and is used as such.
	 *
	 * @param hdd the hard disk to be used for read/write operations.
	 @ @param inode the inode of the file whose block addresses you want.
	 */
	FileBlocks(HDD* hdd, Inode* inode);

	/**
	 * The destructor of the class. This destructor does not free either
	 * the <code>hdd</code> or the <code>inode</code> received as parameters
	 * in the constructor.
	 */
	~FileBlocks();

	/**
	 * Returns the address of the <code>index</code>th block of data of the file.
	 *
	 * The method throws:
	 * <ul>
	 * <li> ArrayIndexOutOfBoundsException* if index is less than 0 (which is rather
	 *	difficult, since it's an unsigned long) or greater than or equal to
	 *	include/defs.h::MAX_BLOCKS.</li>
	 * <li> HardDiskNotInitializedException* forwarded from <code>hdd</code>.</li>
	 * <li> IOException* forwarded from <code>hdd</code>.</li>
	 * <li> InvalidBlockNumberException* forwarded from <code>hdd</code>.</li>
	 * </ul>
	 *
	 * @param index the index of the file's block whose address you want.
	 * @return the address of the <code>index</code>th block of data of the file.
	 */
	unsigned long getBlock(unsigned long index) throw(
		ArrayIndexOutOfBoundsException*,
		HardDiskNotInitializedException*,
		InvalidBlockNumberException*,
		IOException*);

	/**
	 * Sets the address of the <code>index</code>th block of data of the file
	 * to <code>blockAddress</code>. Beware that the method does not allocate
	 * space for the indirection blocks. It simply assumes that the index you
	 * provide is an index in an already allocated & written in the inode
	 * structure indirection block.
	 *
	 * Remeber to write the inode on the disk after calling this method.
	 *
	 * The method throws:
	 * <ul>
	 * <li> ArrayIndexOutOfBoundsException* if index is less than 0 (which is rather
	 *	difficult, since it's an unsigned long) or greater than or equal to
	 *	include/defs.h::MAX_BLOCKS.</li>
	 * <li> HardDiskNotInitializedException* forwarded from <code>hdd</code>.</li>
	 * <li> IOException* forwarded from <code>hdd</code>.</li>
	 * <li> InvalidBlockNumberException* forwarded from <code>hdd</code>.</li>
	 * </ul>
	 *
	 * @param index the index of the file's block whose address you want to change.
	 * @param blockAddress the block address you want to set the <code>index</code>th
	 *	block address to.
	 */
	void setBlock(unsigned long index, unsigned long blockAddress) throw(
		ArrayIndexOutOfBoundsException*,
		HardDiskNotInitializedException*,
		InvalidBlockNumberException*,
		IOException*);

	/**
	 * Returns the addresses of <code>length</code> blocks of data of
	 * the file starting with <code>beginIndex</code>. The array will contain
	 * <code>length</code> elements.
	 *
	 * The method throws:
	 * <ul>
	 * <li> ArrayIndexOutOfBoundsException* if <code>beginIndex</code>+<code>length</code>
	 *	is less than 0 (which is rather difficult, since they're both unsigned longs)
	 *	or greater than or equal to include/defs.h::MAX_BLOCKS.</li>
	 * <li> HardDiskNotInitializedException* forwarded from <code>hdd</code>.</li>
	 * <li> IOException* forwarded from <code>hdd</code>.</li>
	 * <li> InvalidBlockNumberException* forwarded from <code>hdd</code>.</li>
	 * </ul>
	 *
	 * @param beginIndex the index of the first block whose address you want.
	 * @param length the number of blocks you want.
	 * @return an array containing the addresses of <code>length</code> blocks
	 *	of data starting with <code>beginIndex</code>.
	 */
	unsigned long* getBlocks(unsigned long beginIndex, unsigned long length) throw(
		ArrayIndexOutOfBoundsException*,
		HardDiskNotInitializedException*,
		InvalidBlockNumberException*,
		IOException*);

	/**
	 * Sets the addresses of <code>length</code> blocks of data of
	 * the file starting with <code>beginIndex</code> to the values
	 * contained in the <code>addresses</code> array. The array should
	 * therefore contain at least <code>length</code> valid elements.
	 *
	 * Remeber to write the inode on the disk after calling this method.
	 *
	 * The method throws:
	 * <ul>
	 * <li> ArrayIndexOutOfBoundsException* if <code>beginIndex</code>+<code>length</code>
	 *	is less than 0 (which is rather difficult, since they're both unsigned longs)
	 *	or greater than or equal to include/defs.h::MAX_BLOCKS.</li>
	 * <li> HardDiskNotInitializedException* forwarded from <code>hdd</code>.</li>
	 * <li> IOException* forwarded from <code>hdd</code>.</li>
	 * <li> InvalidBlockNumberException* forwarded from <code>hdd</code>.</li>
	 * </ul>
	 *
	 * @param beginIndex the index of the first block whose address you want.
	 * @param length the number of blocks you want.
	 * @param addresses the values of the block address you want to set.
	 */
	void setBlocks(unsigned long beginIndex, unsigned long length, unsigned long* addresses) throw(
		ArrayIndexOutOfBoundsException*,
		HardDiskNotInitializedException*,
		InvalidBlockNumberException*,
		IOException*);

private:
	HDD* hdd;			/* the hard disk used for reading/writing indirection blocks */
	Inode* inode;			/* the inode of the file */
	unsigned char* address;		/* a BLOCK_DIM byte buffer */
};

#endif
