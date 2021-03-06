/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Mon Nov 4 2002
 * Description:		This is the interface of the HDDDriverCache class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _HDD_DRIVER_CACHE_H
#define _HDD_DRIVER_CACHE_H

#include "HDD.h"
#include "HDDDriver.h"
#include "../exceptions/InvalidBlockNumberException.h"
#include "../exceptions/FileNotFoundException.h"
#include "../exceptions/InvalidHDDFileSizeException.h"
#include "../exceptions/HardDiskNotInitializedException.h"

/**
 * This class provides caching for a HDDDriver hard disk. The current
 * implementation provides no cache functionality whatsoever, it simply
 * forwards all requests to the hard disk. It is meant to be implemented
 * later in the development process.
 *
 * @short This class provides caching for a HDDDriver object.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */
class HDDDriverCache : public HDD
{

public:
	/**
	 * Creates a new HDDDriverCache for hard disk <code>hdd</code>.
	 */
	HDDDriverCache(HDDDriver* hdd);

	/**
	 * The destructor of the class.
	 */
	virtual ~HDDDriverCache();

	/**
	 * Reads a block of data from cache. The <code>data</code>
	 * parameter has to have enough space allocated to store
	 * include/defs.h::BLOCK_DIM bytes. If block exists in cache, then
	 * it is served directly from there, otherwise, the block is read
	 * from the hard disk, stored into cache and served.
	 *
	 *
	 * The method throws:
	 * <ul>
	 * <li>	InvalidBlockNumberException* if <code>blockNo</code> is
	 * less than 0 (which is rather impossible since blockNo is an
	 * unsigned long, but anyway...) or <code>blockNo</code> is greater
	 * than or equal to include/defs.h::NR_OF_BLOCKS.</li>
	 * <li> HardDiskNotInitializedException* either if initializeHDD()
	 * hasn't been called yet or if it has been called but it has not
	 * ended successfully (i.e. without exceptions).</li>
	 * <li> IOException* if the read operation fails.</li>
	 * </ul>
	 *
	 * @param blockNo the number of the block to be read (counting starts
	 *	from 0).
	 * @param data the place where to store the read bytes.
	 */
	virtual void readHDDBlock(unsigned long blockNo, unsigned char* data)
		throw(InvalidBlockNumberException*, HardDiskNotInitializedException*, IOException*);

	/**
	 * Writes a block of data to the harddisk. include/defs.h::BLOCK_DIM
	 * bytes will be written from the <code>data</code> parameter, therefore
	 * at least its first include/defs.h::BLOCK_DIM bytes should be valid.
	 * Normally, it just writes the block in cache, making sure that it will
	 * eventually be updated on the hard disk as well.
	 *
	 * The method throws:
	 * <ul>
	 * <li>	InvalidBlockNumberException* if <code>blockNo</code> is
	 * less than 0 (which is rather impossible since blockNo is an
	 * unsigned long, but anyway...) or <code>blockNo</code> is greater
	 * than or equal to include/defs.h::NR_OF_BLOCKS.</li>
	 * <li> HardDiskNotInitializedException* either if initializeHDD()
	 * hasn't been called yet or if it has been called but it has not
	 * ended successfully (i.e. without exceptions).</li>
	 * <li> IOException* if the write operation fails.</li>
	 * </ul>
	 *
	 * @param blockNo the number of the block to be written (counting starts
	 *	from 0).
	 * @param data the data to be written.
	 */
	virtual void writeHDDBlock(unsigned long blockNo, unsigned char* data)
		throw(InvalidBlockNumberException*, HardDiskNotInitializedException*, IOException*);

private:
	HDDDriver* hdd;
};

#endif
