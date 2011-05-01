/*
 * Project:	File System Simulator
 * Author:	Bogdan DUMITRIU
 * E-mail:	bdumitriu@bdumitriu.ro
 * Date:	Sat Nov 2 2002
 * Description:	This is the interface of the HDDDriver class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/


#ifndef _HDD_DRIVER_H
#define _HDD_DRIVER_H

#include <stdlib.h>
#include <string.h>
#include <iostream.h>
#include <fstream.h>
#include "../defs.h"
#include "HDD.h"
#include "../exceptions/InvalidBlockNumberException.h"
#include "../exceptions/FileNotFoundException.h"
#include "../exceptions/InvalidHDDFileSizeException.h"
#include "../exceptions/HardDiskNotInitializedException.h"

/**
 * This is the only class which directly interacts with the hard disk file.
 * Its purpose is to provide block read & write access to the hard disk file
 * in the same manner as a regular hdd does.
 *
 * @short This class provides block read/write access to the hdd file.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */
class HDDDriver : public HDD
{

public:
	/**
	 * Creates a new HDDDriver object using include/defs.h::HDD_FILENAME
	 * as the hard disk file.
	 */
	HDDDriver();

	/**
	 * Creates a new HDDDriver object using <code>hddFileName</code>
	 * as the hard disk file.
	 *
	 * @param hddFileName the name of the hard disk file.
	 */
	HDDDriver(const char* hddFileName);

	/**
	 * The destructor of the class.
	 */
	virtual ~HDDDriver();

	/**
	 * Does some sanity checks and, if all goes well, opens the hard disk
	 * file for read/write operations. This method *must* be called and must
	 * run without throwing exceptions in order for read/write operations to work.
	 *
	 * The method throws:
	 * <ul>
	 * <li>	FileNotFoundException* if hard disk file cannot be found on
	 *	the disk.</li>
	 * <li> InvalidHDDFileException* if (include/defs.h::NR_OF_BLOCKS)
	 *	multiplied by (include/defs.h::BLOCK_DIM) is different from
	 *	the hard disk file size in bytes.</li>
	 * </ul>
	 */
	void initializeHDD() throw(FileNotFoundException*, InvalidHDDFileSizeException*);

	/**
	 * Reads a block of data from the harddisk. The <code>data</code>
	 * parameter has to have enough space allocated to store
	 * include/defs.h::BLOCK_DIM bytes.
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
	 * <li> FileNotFoundException* if hard disk file cannot be found on
	 *	the disk.</li>
	 * </ul>
	 *
	 * @param blockNo the number of the block to be read (counting starts
	 *	from 0).
	 * @param data the place where to store the read bytes.
	 */
	virtual void readHDDBlock(unsigned long blockNo, unsigned char* data) throw(
		InvalidBlockNumberException*,
		HardDiskNotInitializedException*,
		IOException*,
		FileNotFoundException*);

	/**
	 * Writes a block of data to the harddisk. include/defs.h::BLOCK_DIM
	 * bytes will be written from the <code>data</code> parameter, therefore
	 * at least its first include/defs.h::BLOCK_DIM bytes should be valid.
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
	char* fileName;		/* the hard disk file */
	ifstream hddIn;		/* the stream used to read from file */
	ofstream hddOut;	/* the stream used to write to file */
	bool initOk;		/* flag to see whether hard disk has been properly initialized */
};

#endif
