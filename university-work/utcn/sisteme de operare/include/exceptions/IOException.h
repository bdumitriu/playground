// Project: os file system
// Creation date: Oct 19, 2002
// Author: andi
// Modifications: bogdan
// Modification date: Oct 21, 2002
// File name: IOException.h
// Description: ...

#ifndef _IO_EXCEPTION_H
#define _IO_EXCEPTION_H

#define HARD_DISK_FAILURE	1	/* hard disk file doesn't exist, etc. */
#define HARD_DISK_CORRUPTED	2	/* hard disk is inconsistent, etc. */

class IOException
{

public:
	/**
	 * Returns a string containing a description of this IOException.
	 * @return a string containing a description of this IOException.
	 */
	virtual char* getMessage();

	/**
	 * Returns a number which identifies the type of IOException.
	 * @return an int which identifies the type of IOException.
	 */
	virtual int getErrorCode();

private:
	char* message;
	int errorCode;
};

#endif
