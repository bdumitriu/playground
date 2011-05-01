/*
 * Project:		File System Simulator
 * Author:		Bogdan DUMITRIU
 * E-mail:		bdumitriu@bdumitriu.ro
 * Date:		Sun Oct 27 2002
 * Description:		This header contains system wide definitions.
 */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef _DEFS_H
#define _DEFS_H

/*************************
 * file type definitions *
 *************************/

/**
 * the type value for a normal file
 */
#define NORMAL_FILE_TYPE 1

/**
 * the type value for a directory
 */
#define FOLDER_FILE_TYPE 2

/*************************
 * file type definitions *
 *************************/

/**
 * the path separator character
 */
#define PATH_SEPARATOR '/'

/***********************************
 * hard disk structure definitions *
 ***********************************/

/**
 * the name of the hard disk file
 */
#define HDD_FILENAME "hdd.sim"

/**
 * the dimension of a block in bytes
 */
#define BLOCK_DIM 512			/* must be >= 512 and multiple of 4 */

/**
 * the number of available inodes (file attributes structures)
 */
#define MAX_NO_FILEATTR 200		/* must be between 3 and 8*(BLOCK_DIM-8) */

/**
 * the total number of blocks
 */
#define NR_OF_BLOCKS 100000		/* must be >= MAX_NO_FILEATTR+FILEATTR_AREA_START_ADDR-1 */
					/* must be >= MAX_NO_FILEATTR+FILEATTR_AREA_START_ADDR-1 */


/**
 * the first block containing an inode (count starts from 0)
 */
#define FILEATTR_AREA_START_ADDR 2	/* must be between 2 and NR_OF_BLOCKS-MAX_NO_FILEATTR */

/**
 * the number of the inode associated to the free blocks file
 */
#define FREEBLOCKS_FILEATTR 198		/* must be between 0 and MAX_NO_FILEATTR-2 */
					/* this is because MAX_NO_FILEATTR-1 is reserved */

/**
 * the number of the inode associated to the root directory
 */
#define ROOT_DIR_FILEATTR 0		/* must be between 0 and MAX_NO_FILEATTR-2 */
					/* this is because MAX_NO_FILEATTR-1 is reserved */

/**
 * the number of direct block addresses in an inode
 */
#define DIRECT_BLOCKADDR_ENTRIES 15	/* must be between 0 and (BLOCK_DIM-180)/4 */

/**
 * the maximum number of blocks a file can occupy
 */
#define MAX_BLOCKS DIRECT_BLOCKADDR_ENTRIES+(((BLOCK_DIM-180-DIRECT_BLOCKADDR_ENTRIES*4)/4)*(BLOCK_DIM/4))

/**
 * the maximum size of a file
 */
#define MAX_FILESIZE MAX_BLOCKS*BLOCK_DIM

/***************
 * error codes *
 ***************/

/**
 * error code returned when the path you provide doesn't exist
 * In the special case of the rename file method, this is returned
 * if the original file path is invalid. (all methods which take a
 * path as parameter).
 */
#define INVALID_SOURCE_PATH 1

/**
 * error code returned when you want to create a file/directory
 * with a name that already exists (create file, rename file)
 */
#define DUPLICATE_FILE_NAME 2

/**
 * error code returned when the number of bytes you want to write
 * in a file would cause the file size to be larger than the maximum
 * allowed size. Since directories are stored as files, this error
 * code can also be returned while trying to create a file or to
 * rename a file and the directory would become larger than allowed.
 * It could even by returned by the formatHDD method if there isn't
 * enough space in the inode to store the free blocks bitmap
 * (write file, create file, rename file, formatHDD)
 */
#define FILE_SIZE_TOO_LARGE 3

/**
 * error code returned when there is not enough space on the
 * hard disk to perform operation. This doesn't necessarily
 * mean that the hard disk is actually full. It just means
 * there isn't enough space to write what you want to write
 * (write file, create file, reaname file, formatHDD)
 */
#define HARD_DISK_FULL 4

/**
 * error code returned when there are no more inodes available to
 * store new files/directories (create file)
 */
#define NO_INODE_AVAILABLE 5

/**
 * error code returned when the hard disk is corrupted (all
 * methods)
 */
#define HARD_DISK_CORRUPTED 6

/**
 * error code returned when you try to read past the end of
 * a file (read file)
 */
#define NOT_ENOUGH_BYTES 7

/**
 * error code returned by the rename file method when
 * the new file path does not exist (rename file)
 */
#define INVALID_DESTINATION_PATH 8

/**
 * error code returned by all methods if they are called
 * prior to a successful call to initializeDriver() (all
 * methods)
 * .
 */
#define DRIVER_NOT_INITIALIZED 9

/**
 * error code returned by the initializeDriver() method
 * if the hard disk file cannot be found (initializeDriver)
 */
#define HARD_DISK_MISSING 10

/**
 * error code returned by the initializeDriver() method
 * if the hard disk structure definitions in include/defs.h
 * do not comply with certain restrictions (initializeDriver)
 */
#define BAD_HDD_GEOMETRY 11

/**
 * error code returned by all methods which want to use
 * the hard disk if it is not formatted
 */
#define HARD_DISK_NOT_FORMATTED 12

/**
 * error code returned by all methods which take a path
 * argument when this argument doesn't start with PATH_SEPARATOR
 */
#define NOT_AN_ABSOLUTE_PATH 13

/**
 * error code returned by all methods which take a path
 * argument when the last file/directory name in the path
 * is invalid
 */
#define INVALID_FILE_NAME 14

#endif

/*
	all > 0
	BLOCK_DIM = multiple of 4
	BLOCK_DIM >= 180 + DIRECT_BLOCKADDR_ENTRIES*4
	FILEATTR_AREA_START_ADDR >= 2
	MAX_NO_FILEATTR <= NR_OF_BLOCKS-FILEATTR_AREA_START_ADDR
	MAX_NO_FILEATTR <= (BLOCK_DIM-20)*8
	MAX_NO_FILEATTR >= 3
	NR_OF_BLOCKS >= FILEATTR_AREA_START_ADDR + MAX_NO_FILEATTR
	FREEBLOCKS_FILEATTR, ROOT_DIR_FILEATTR <= MAX_NO_FILEATTR-1
*/
