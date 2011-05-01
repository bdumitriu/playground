/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Mon Nov 4 2002
 * Description:		This program creates an empty hard disk with
 *			a size of NR_OF_BLOCKS*BLOCK_DIM.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _CREATE_HDD
#define _CREATE_HDD

#include <fstream.h>
#include "include/defs.h"
	
int main(void)
{
	char zero[1];
	zero[0] = 0x00;

	ofstream file;
	file.open(HDD_FILENAME, ios::out | ios::binary);
	if (file.fail())
	{
		cout << "Couldn't create file." << endl;
		exit(1);
	}
	file.seekp(NR_OF_BLOCKS*BLOCK_DIM-1);
	file.write(zero, 1);
	file.close();
	cout << "File created." << endl;
}

#endif
