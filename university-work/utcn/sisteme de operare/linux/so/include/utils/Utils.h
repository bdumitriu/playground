/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Mon Nov 4 2002
 * Description:		This is the interface of the Utils class.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _UTILS_H
#define _UTILS_H

#include <string.h>
#include <stdlib.h>
#include <iostream.h>

typedef union
{
	unsigned char bytes[4];
	unsigned long number;
} BytesULong;

typedef union
{
	unsigned char bytes[2];
	unsigned short number;
} BytesUShort;

/**
 * Class Utils contains all sort of useful static methods.
 *
 * @short Contains all sort of useful static methods.
 * @author Bogdan DUMITRIU
 * @version 0.1
 */
class Utils
{
public:
	/**
	 * Returns the unsigned long value represented by the 4 bytes.
	 *
	 * @param bytes the byte representation of the unsigned long number.
	 * @return the unsigned long value represented by the 4 bytes.
	 */
	static unsigned long bytesToULong(unsigned char bytes[4]);

	/**
	 * Returns the 4 byte representation of the unsigned long value
	 * <code>number</code>.
	 *
	 * @param number the long value to be converted.
	 * @return the 4 byte representation of the unsigned long value
	 *	<code>number</code>.
	 */
	static unsigned char* uLongToBytes(unsigned long number);

	/**
	 * Returns the unsigned short value represented by the 2 bytes.
	 *
	 * @param bytes the byte representation of the unsigned short number.
	 * @return the unsigned short value represented by the 2 bytes.
	 */
	static unsigned short bytesToUShort(unsigned char bytes[2]);

	/**
	 * Returns the 2 byte representation of the unsigned short value
	 * <code>number</code>.
	 *
	 * @param number the short value to be converted.
	 * @return the 2 byte representation of the unsigned short value
	 *	<code>number</code>.
	 */
	static unsigned char* uShortToBytes(unsigned short number);
};

#endif
