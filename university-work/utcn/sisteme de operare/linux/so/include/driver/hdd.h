/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Mon Nov 4 2002
 * Description:		This is the interface of the HDD class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _HDD_H
#define _HDD_H

/**
 * This is the base class for any class which wants to provide
 * some sort of hard disk functionaliy. It is an abstract class.
 *
 * @short This class provides a common interface to hdd operations.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */
class HDD
{
public:
	/**
	 * Reads a block of data from the harddisk.
	 *
	 * @param blockNo the number of the block to be read.
	 * @param data the place where to store the read bytes.
	 */
	virtual void readHDDBlock(unsigned long blockNo, unsigned char* data) = 0;

	/**
	 * Writes a block of data to the harddisk.
	 *
	 * @param blockNo the number of the block to be written.
	 * @param data the data to be written.
	 */
	virtual void writeHDDBlock(unsigned long blockNo, unsigned char* data) = 0;

};

#endif
