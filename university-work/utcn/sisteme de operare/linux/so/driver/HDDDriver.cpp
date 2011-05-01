/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Sat Nov 2 2002
 * Description:		This is the implementation of the HDDDriver class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "../include/driver/HDDDriver.h"

HDDDriver::HDDDriver()
{
	fileName = new char[strlen(HDD_FILENAME)+1];
	if (fileName == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	strcpy(fileName, HDD_FILENAME);
	initOk = false;
}

HDDDriver::HDDDriver(const char* fileName)
{
	this->fileName = new char[strlen(fileName)+1];
	if (this->fileName == NULL)
	{
		cout << "Not enough memory.\n";
		exit(1);
	}
	strcpy(this->fileName, fileName);
	initOk = false;
}

HDDDriver::~HDDDriver()
{
	if (fileName != NULL)
	{
		delete [] fileName;
	}
	hddOut.close();
	delete &hddIn;
	delete &hddOut;
}

void HDDDriver::initializeHDD() throw(FileNotFoundException*, InvalidHDDFileSizeException*)
{
	// if it's already initialized
	if (initOk)
	{
		return;
	}

	// open fileName in write mode (without erasing it)
	hddOut.open(fileName, ios::out | ios::ate | ios::binary);
	if (hddOut.fail())
	{
		hddOut.close();
		initOk = false;
		throw new FileNotFoundException(fileName);
	}

	// see if hard disk file size matches NR_OF_BLOCKS*BLOCK_DIM
	if (hddOut.tellp() != NR_OF_BLOCKS*BLOCK_DIM)
	{
		InvalidHDDFileSizeException* e = new InvalidHDDFileSizeException(hddOut.tellp());
		hddOut.close();
		initOk = false;
		throw e;
	}

	// if all went ok, we can allow read/write
	initOk = true;
}

void HDDDriver::readHDDBlock(unsigned long blockNo, unsigned char* data) throw(
	InvalidBlockNumberException*,
	HardDiskNotInitializedException*,
	IOException*,
	FileNotFoundException*)
{
	// check to see if hard disk has been initialized
	if (!initOk)
	{
		throw new HardDiskNotInitializedException();
	}

	// check to see if blockNo is legal
	if (blockNo >= NR_OF_BLOCKS)
	{
		throw new InvalidBlockNumberException(blockNo);
	}

	// open fileName in read mode
	hddIn.open(fileName, ios::in | ios::binary);
	if (hddIn.fail())
	{
		hddOut.close();
		initOk = false;
		throw new FileNotFoundException(fileName);
	}

	// clear state
	hddIn.clear();

	// set file pointer at the beginning of the requested block
	hddIn.seekg(blockNo*BLOCK_DIM);

	// read BLOCK_DIM bytes into data
	hddIn.read(data, BLOCK_DIM);

	// if read failed...
	if (hddIn.fail())
	{
		hddIn.close();
		throw new IOException("Failed to read form hard disk.");
	}

	// close the file
	hddIn.close();
}

void HDDDriver::writeHDDBlock(unsigned long blockNo, unsigned char* data)
	throw(InvalidBlockNumberException*, HardDiskNotInitializedException*, IOException*)
{
	// check to see if hard disk has been initialized
	if (!initOk)
	{
		throw new HardDiskNotInitializedException();
	}

	// check to see if blockNo is legal
	if (blockNo >= NR_OF_BLOCKS)
	{
		throw new InvalidBlockNumberException(blockNo);
	}

	// clear state
	hddOut.clear();

	// set file pointer at the beginning of the requested block
	hddOut.seekp(blockNo*BLOCK_DIM);

	// write BLOCK_DIM bytes into data
	hddOut.write(data, BLOCK_DIM);

	// if write failed...
	if (hddOut.fail())
	{
		throw new IOException("Failed to write to hard disk.");
	}
	else
	{
		hddOut.flush();
	}
}
