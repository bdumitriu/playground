
/**
 * Just some definitions used pretty much everywhere and 
 * the assert macro.
 *
 * @Author Tudor Marian
 *
 * Technical University Of Cluj-Napoca
 * Computer Science Departament
 * gr. 3241/1
 *
 * created: april, 13, 2003
 * last modified: april 19th, 2003
 */

#ifndef _MY_DEBUG_H_
#define _MY_DEBUG_H_

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <iostream.h>

typedef unsigned char byte;
typedef unsigned long dword;
typedef unsigned short word;

union dword_byte
{
	byte bytes[4];
	dword val;
};

union word_byte
{
	byte bytes[2];
	word val;
};

union rgb_byte
{
	byte bytes[3];
	dword val;
};

#define RGB_SIZE 3
#define WORD_SIZE 2
#define DWORD_SIZE 4

#define ASSERT(condition) \
	if (condition) \
	{} \
	else \
	{ \
		cerr << "Assertion failed: line " << __LINE__ << " file " << __FILE__ << "\n"; \
        exit(1); \
    }

#endif //_MY_DEBUG_H_

