

/**
 * @Author Tudor Marian
 *
 * Technical University Of Cluj-Napoca
 * Computer Science Departament
 * gr. 3241/1
 *
 * created: april, 13, 2003
 * last modified: april 19, 2003
 *
 */

#include "LZWCompressor.h"

void LZWCompressor::setDefaultPolicies()
{
	setCompressedDataDeletePolicy(doDelete);
	setRawDataDeletePolicy(doDelete);
}

/**
 * LZWCompressor::LZWCompressor
 *	Creates an object.
 */
LZWCompressor::LZWCompressor()
{
	rawData = NULL;
	compressedData = NULL;

	// create the list of listening objects
	listeners = new List<LZWCompressionListener*>();

	setStartNoOfBits(INIT_BITS-1);

	// set default delete policies
	setDefaultPolicies();
}

/**
 * LZWCompressor::LZWCompressor
 *	Creates an object and sets the data buffer according to the
 *	<code>operation</code> parameter.
 */
LZWCompressor::LZWCompressor(int operation, ByteBuffer* data)
{
	OPERATION_TYPE = operation;
	ASSERT(OPERATION_TYPE == COMPRESS_OPERATION || 
		OPERATION_TYPE == EXPAND_OPERATION);

	if (OPERATION_TYPE == COMPRESS_OPERATION)
	{
		compressedData = new ByteBuffer();
		rawData = data;
	}
	else
	{
		rawData = new ByteBuffer();
		compressedData = data;
	}

	// create the list of listening objects
	listeners = new List<LZWCompressionListener*>();
	setStartNoOfBits(INIT_BITS-1);

	// set default delete policies
	setDefaultPolicies();
}

LZWCompressor::LZWCompressor(ByteBuffer* rawData, ByteBuffer* compressedData)
{
	this->rawData = rawData; 
	this->compressedData = compressedData;
	listeners = new List<LZWCompressionListener*>();
	setStartNoOfBits(INIT_BITS-1);
	
	// set default delete policies
	setDefaultPolicies();
}

/**
 * LZWCompressor::~LZWCompressor
 *	Destroys the object.
 */
LZWCompressor::~LZWCompressor()
{
	if ((rawData != NULL) && (rawDataDeletePolicy == doDelete))
		delete rawData;
	if ((compressedData != NULL) && (compressedDataDeletePolicy == doDelete))
		delete compressedData;

	delete listeners;
}

void LZWCompressor::setStartNoOfBits(int startNoOfBits)
{
	if (startNoOfBits < 2 || startNoOfBits > 9) // must have bounds
		this->startNoOfBits = -1;
	else
		this->startNoOfBits = startNoOfBits;
}

void LZWCompressor::setCompressedDataDeletePolicy(DeletePolicy policy)
{
	compressedDataDeletePolicy = policy;
}

void LZWCompressor::setRawDataDeletePolicy(DeletePolicy policy)
{
	rawDataDeletePolicy = policy;
}

/**
 * LZWCompressor::setCompressedData
 *	Sets the compressed data buffer.
 */
void LZWCompressor::setCompressedData(ByteBuffer* compressedData) 
{
	if (this->compressedData != NULL)
		delete this->compressedData; 
	this->compressedData = compressedData;
}

/**
 * LZWCompressor::setRawData
 *	Sets the raw data buffer.
 */
void LZWCompressor::setRawData(ByteBuffer* rawData)
{
	if (this->rawData != NULL)
		delete this->rawData;
	this->rawData = rawData;
}

/**
 * LZWCompressor::findIndexFromHashtable
 *	Computes the hash code wich is considered to be an index into
 *	a closed hash table.
 *	@returns the index in the hash table
 */
unsigned short LZWCompressor::findIndexFromHashtable(unsigned short hashPrefix, 
		unsigned short hashCharacter)
{
	int index; // the index to be found
	unsigned short offset; // used to toy the closed hash table

	// simple XOR operation on a closed hash table

	index = (hashCharacter << HASHING_SHIFT_BITS) ^ hashPrefix;
	
	if (index == 0)
	{
		offset = 1;
	}
	else
	{
		offset = TABLE_SIZE - index;
	}

	// start searching
	while (1)
	{
		if (codeValue[index] == -1) // found an empty space where elements are supposed to be
		{
			return index;
		}
		// found the existing one in place
		if ((prefixCode[index] == hashPrefix) && 
			(appendCharacter[index] == hashCharacter))
		{
			return index;
		}

		index -= offset;
		if (index < 0)
		{
			index += TABLE_SIZE;
		}
	}
}

/**
 * LZWCompressor::outputCode
 *	Performs the packing procedure of a compression code value of 
 *	numBits size to normal bytes as follows:
 *		considering a code represented on numBits = 9 bits
 *		it will be represented such as:
 *			code1 = aaaaaaaaa
 *			code2 = bbbbbbbbb
 *			output: aaaaaaaaa
 *					bbbbbbbba
 *					*******bb and so on
 *		Consider this example to be the startpoint for the variable
 *		bit-length codes.					
 */
void LZWCompressor::outputCode(unsigned short code)
{
	ASSERT(OPERATION_TYPE == COMPRESS_OPERATION);
/*
	LITTLE ENDIAN STYLE
	static int output_bit_count = 0; // a static variable helding the
									// byte we've reached

	static unsigned long output_bit_buffer = 0L;

	output_bit_buffer |= (unsigned long) code << (32 - numBits - output_bit_count);

	output_bit_count += numBits;

	while (output_bit_count >= 8)
	{
		//compressedData->addByte((byte) (output_bit_buffer >> 24));
		// replaced by:
		*compressedData << ((byte) (output_bit_buffer >> 24));

		output_bit_buffer <<= 8;
		output_bit_count -= 8;

		bytesOut ++; // for compression monitoring
	}
*/
/*
	BIG ENDIAN STZYLE
	static int noOfBitsLeft = 0;

	static unsigned long output_bit_buffer = 0L;

	output_bit_buffer |= (unsigned long) code << (noOfBitsLeft);

	noOfBitsLeft += numBits;

	while (noOfBitsLeft >= 8)
	{
		compressedData->addByte((byte) (output_bit_buffer));
		
		output_bit_buffer >>= 8;

		noOfBitsLeft -= 8;

		bytesOut ++;	
	}
*/
	outputBitBuffer &= code_mask[bitCount];
	
	if (bitCount > 0)
	{
		outputBitBuffer |= (code << bitCount);
	}
	else
	{
		outputBitBuffer = code;
	}
	bitCount += numBits;

	while (bitCount >= 8)
	{
			compressedData->addByte((byte) (outputBitBuffer & 0xff));
			outputBitBuffer >>= 8;
			bitCount -= 8;

			bytesOut++;
	}

	// If the next entry is going to be too big for the code size,
	// then increase it, if possible.
	if (nextCode > maxCode || clearFlag)
	{
		if (clearFlag)
		{
			maxCode = MAXVAL(numBits = initBits);
			clearFlag = false;

			// fire the event listeners
			event = new LZWCompressionEvent(CLEARING_TABLE);
			fireEvent(event);
			delete event;
		}
		else
		{
			++numBits;
			
			if (numBits == maxBits)
				maxCode = upperBoundCode;
			else
				maxCode = MAXVAL(numBits);

			// fire the event listeners
			event = new LZWCompressionEvent(INCREMENTING_CODE_SIZE);
			fireEvent(event);
			delete event;
		}
	}

	if (code == terminatorCode) // flush the rest of the outputBitBuffer
	{
		while (bitCount > 0)
		{
			compressedData->addByte((byte) (outputBitBuffer & 0xff));
			outputBitBuffer >>= 8;
			bitCount -= 8;

			bytesOut ++;
		}
	}
}

/**
 * LZWCompressor::inputCode
 *	The method reads a code from a stream of bytes, and converts it to
 *	the numBits format. Please see the pair method LZWCompressor::outputCode
 *	for further reference, the present one does the reverse operation
 *	the upper mentioned does.
 */
unsigned short LZWCompressor::inputCode()
{
	ASSERT(OPERATION_TYPE == EXPAND_OPERATION);
/*
	unsigned short return_value;
	static int input_bit_count = 0;
	static unsigned long input_bit_buffer = 0L;

	while (input_bit_count <= 24)
	{
		input_bit_buffer |= (unsigned long) compressedData->getByte() << (24 - input_bit_count);
		input_bit_count += 8;
	}

	return_value = (unsigned short) (input_bit_buffer >> (32 - numBits));
	input_bit_buffer <<= numBits;
	input_bit_count -= numBits;

	return return_value;
*/
	unsigned long ret;

	if (noOfBitsLeft == 0)
	{
		octet = compressedData->getByte();
		
		noOfBitsLeft = 8;
	}

	ret = octet >> (8 - noOfBitsLeft);

	while (numBits > noOfBitsLeft)
	{
		octet = compressedData->getByte();

		ret |= octet << noOfBitsLeft;
		
		noOfBitsLeft += 8;
	}

	noOfBitsLeft -= numBits;

	ret &= code_mask[numBits];

	return ((unsigned short) (ret));

}


/**
 * LZWCompressor::decodeString
 *	The method uses a stack to backtrace the string elements that
 *	have been given a code (the index of the hash table).
 */
byte* LZWCompressor::decodeString(byte* buffer, unsigned short code)
{
	int i = 0;
	
	// as long as the codes are not bytes
	while (code >= firstCode)
	{
		// push a new element in reversed order within the stack
		*buffer++ = appendCharacter[code];
		code = prefixCode[code];

		i++;

		ASSERT(i < STACK_SIZE);
	}

	*buffer = (unsigned char)code; // at his point it is a byte

	return buffer; // at this point the stack pointer has advanced i positions
					// and will be compared with the calling stack_poiter, 
					// except ofcourse the special case
}


/**
 * LZWCompressor::expand
 *	The method expands the byte array found into the compressedData
 *	buffer, and stores it into the rawData buffer. It uses the Lempel
 *	Ziv method, with variable code length and also clearing table.
 *	When finished it trims the result buffer to just it's size.
 */
void LZWCompressor::expand2()
{
	init();

	// allocate the recources
	prefixCode = new unsigned short[TABLE_SIZE];
	appendCharacter = new byte[TABLE_SIZE];
	decodeStack = new byte[STACK_SIZE];

	OPERATION_TYPE = EXPAND_OPERATION;

	// reset the seeker of the raw data buffer
	rawData->resetSeeker();
	// and the same for the compressed data buffer
	compressedData->resetSeeker();

	maxCode = MAXVAL(numBits);

	unsigned short nextCode = firstCode;
	unsigned short newCode;
	unsigned short oldCode;

	short character;
	int counter = 0;

	bool clearFlag = false; // need to clear the value array

	unsigned char* string;
	
	// notify the start of the expanding operation
	LZWCompressionEvent* event = new LZWCompressionEvent(OPERATION_STARTED);
	fireEvent(event);
	delete event;

	/* main loop */
	while ((newCode = inputCode()) != TERMINATOR)
	{
		if (clearFlag)
		{
			clearFlag = false;
			
			oldCode = newCode;
			character = oldCode;

//			rawData->addByte((byte)oldCode); //replaced with:
			*rawData << (byte) oldCode;

			continue;
		}

		if (newCode == CLEAR_TABLE)
		{
			clearFlag = 1;

			numBits = INIT_BITS;
			nextCode = FIRST_CODE;
			
			// notify the clearing of the code table
			event = new LZWCompressionEvent(CLEARING_TABLE);
			fireEvent(event);
			delete event;

			maxCode = MAXVAL(numBits);
			
			continue;
		}

		if (++ counter == 1000) // the pacifier, every 1000 bytes
		{
			counter = 0;

			// notify periodic pacifier
			event = new LZWCompressionEvent(PERIODIC_COMPRESSION);
			fireEvent(event);
			delete event;
		}

		// eliminate the string+character+string+character+string inconsistency
		if (newCode >= nextCode)
		{
			// the decode_stack is assumed to be insignifiant of content at call time

			// put the first/(last after decoding) element
			*decodeStack = (byte) character;

			// follows some serious pointer arithmetics
			// starting with the second stack slot the decoding is called
			// the first stack slot is held by the character
			string = decodeString(decodeStack+1, oldCode);
		}
		else // do a simple decoding operation - using the stack
		{
			string = decodeString(decodeStack, newCode);
		}

		// remember the first character in string
		character = *string;

		// output backwards the stack
		while (string >= decodeStack) // both the poiters are on the same array
									// only at different locations
		{
			//putc(*string--, output);
//			rawData->addByte(*string--);
			*rawData << (*string--);
		}

		// add new code to the string table
		if (nextCode <= maxCode) 
		{
			prefixCode[nextCode] = oldCode;

			// ath this point the character is just a byte
			appendCharacter[nextCode++] = (unsigned char) character;
			
			if (nextCode == maxCode && numBits < BITS)
			{
				// notify increasing the number of bits of the code
				event = new LZWCompressionEvent(INCREMENTING_CODE_SIZE);
				fireEvent(event);
				delete event;

				maxCode = MAXVAL(++ numBits);
			}
		}

		oldCode = newCode;
	}

	delete decodeStack;
	delete prefixCode;
	delete appendCharacter;

	// trim the rawData buffer
	rawData->trimToSize();
	
	// notify listeners of termination
	event = new LZWCompressionEvent(OPERATION_TERMINATED);
	fireEvent(event);
	delete event;
}


/**
 * LZWCompressor::init
 *	The method initializes the needed fieds.
 */
bool LZWCompressor::init()
{
	if (startNoOfBits < 0)
		return false;
	
	numBits = startNoOfBits + 1;
	maxCode = 1 << numBits;

	initBits = numBits;

	clearCode = 1 << startNoOfBits;
	terminatorCode = clearCode + 1;
	firstCode = terminatorCode + 1;

	bytesIn = 0;
	noOfBitsLeft = 0;
	return true;
}

void LZWCompressor::expand()
{
	if (!init())
		return;

	prefixCode = new unsigned short[TABLE_SIZE];
	appendCharacter = new byte[TABLE_SIZE];
	decodeStack = new byte[STACK_SIZE];

	nextCode = firstCode;
	unsigned short newCode, code, c;
	unsigned short oldCode;
	short character;
	short counter = 0;

	// top of the stack
	unsigned char *sp = decodeStack;

	oldCode = character = 0;

	OPERATION_TYPE = EXPAND_OPERATION;

	// reset the seeker of the raw data buffer
	rawData->resetSeeker();
	// and the same for the compressed data buffer
	compressedData->resetSeeker();
	
	// fire start operation event
	event = new LZWCompressionEvent(OPERATION_STARTED);
	fireEvent(event);
	delete event;
	

	while ((c = inputCode()) != terminatorCode)
	{
		if ((++bytesIn) >= PACIFIER)
		{
			bytesIn = 0;
			event = new LZWCompressionEvent(PERIODIC_COMPRESSION);
			fireEvent(event);
			delete event;
		}

		if (c == clearCode)
		{
			// signal clear event
			event = new LZWCompressionEvent(CLEARING_TABLE);
			fireEvent(event);
			delete event;

			numBits = initBits;
			newCode = firstCode;
			maxCode = 1 << numBits;
			
			while ((c = inputCode()) == clearCode)
			{
				// signal clear event
				event = new LZWCompressionEvent(CLEARING_TABLE);
				fireEvent(event);
				delete event;
			}
			
			if (c == terminatorCode)
				break;
			
			// should NOT hapen, but anyway, cross fingers & hope it works
			if (c >= nextCode)
				c = 0;
         
			oldCode = character = c;

			// add the byte to the decompressed data stream
			*rawData << ((byte) c);
		}
		else
		{
			code = c;
			
			// handle special string+character+string+character+string case
			// and trick the decoder
			if (code >= newCode)
			{
				code = oldCode;
				*sp++ = (byte) character;
            }

			// add up the decoded character to use it further
			while (code >= FIRST_CODE)
			{
				*sp++ = appendCharacter[code];
				code = prefixCode[code];
			}
			
			*sp++ = (unsigned char) code;
         
			/* Now that we've pushed the decoded string (in reverse order) onto
			* the stack, lets pop it off and put it into our decode buffer.
			* And when the decode buffer is full, write another line... */
			while (sp > decodeStack)
            {
				*rawData << (*(--sp));
            }
			
			if (newCode < maxCode)
			{
				character = code;
				appendCharacter[newCode] = (byte) code;
				
				prefixCode[newCode++] = oldCode;
				oldCode = c;
			}
         
			if (newCode == maxCode)
			{
				if (numBits < BITS)
				{
					maxCode <<= 1;
					++numBits;
					
					// increase number of bits event
					event = new LZWCompressionEvent(INCREMENTING_CODE_SIZE);
					fireEvent(event);
					delete event;
				} 
			}
		}
	}

	// fire end operation event
	event = new LZWCompressionEvent(OPERATION_TERMINATED);
	fireEvent(event);
	delete event;

	delete decodeStack;
	delete []prefixCode;
	delete []appendCharacter;
}


/*
void expand2()
{
	// allocate the recources
	prefixCode = new unsigned short[TABLE_SIZE];
	appendCharacter = new byte[TABLE_SIZE];
	decodeStack = new byte[STACK_SIZE];

	OPERATION_TYPE = EXPAND_OPERATION;

	// reset the seeker of the raw data buffer
	rawData->resetSeeker();
	// and the same for the compressed data buffer
	compressedData->resetSeeker();

	unsigned short newCode; // c
	unsigned short oldCode; // oc
	unsigned short code;
	short character; // fc

	int counter = 0;

	unsigned char* string = decodeStack; // the stack pointer

	init();
	oldCode = character = 0;

	while ((newCode = inputCode()) != terminatorCode)
	{
		ASSERT (newCode >= 0);
		
		// If the code is a clear code, reinitialize all necessary items.
		if (newCode == clearCode)
		{
			numBits = startNoOfBits + 1;

			nextCode = firstCode;

			maxCode = MAXVAL(numBits);

			// Continue reading codes until we get a non-clear code
			 // (Another unlikely, but possible case...) 
			while ((newCode = inputCode()) == clearCode)
				;
			// If we get an ending code immediately after a clear code
			//  (Yet another unlikely case), then break out of the loop. 
			if (newCode == terminatorCode)
				break;
			
			 // Finally, if the code is beyond the range of already set codes, 
			 // (this had better NOT happen. I have no idea what will result from
			 // this, but I doubt it will look good) then set it to color zero. 
			if (newCode >= nextCode)
				newCode = 0;
			
			oldCode = character = newCode;
			
			rawData->addByte((byte) newCode);
		}
		else
		{
			code = newCode;

			if (code >= nextCode)
            {
				code = oldCode;
				*string++ = (unsigned char) character;
            }
			
			while (code >= firstCode)
			{
				*string++ = appendCharacter[code];
				code = prefixCode[code];
            }
         
			*string++ = (byte) code;
			
			if (nextCode < maxCode)
            {
				character = code;
				appendCharacter[nextCode] = (byte) code;
				
				prefixCode[nextCode++] = oldCode;

				oldCode = newCode;
            }

			if (nextCode >= maxCode)
				if (numBits < BITS)
				{
					maxCode <<= 1;
					++numBits;
				} 
         
			while (string > decodeStack)
            {
				rawData->addByte((byte) (*(--string)));
            }
         }
      }
}
*/

void LZWCompressor::initCompressParameters()
{
	maxBits = BITS;
	upperBoundCode = 1 << maxBits;
	nextCode = 0;
	clearFlag = false;
	
	if (startNoOfBits == -1)
		initBits = INIT_BITS;
	else
		initBits = startNoOfBits + 1;

	hashTableSize = TABLE_SIZE;

	// clear out the output buffer
	outputBitBuffer = 0;
	bitCount = 0;

	bytesIn = bytesOut = 0;
}

void LZWCompressor::clearHashTable()
{
	for (int i = 0; i < hashTableSize; i++)
		hashTable[i] = -1;
}

/**
 * LZWCompressor::compress
 *	The method compresses the byte array found into the rawData buffer
 *	and stores the result into the compressedData buffer. It uses the
 *	Lempel Ziv compression method with variable code length and clear
 *	code table method.
 *	When done it trims the compressedData buffer to it's size.
 */

void LZWCompressor::compress()
{
//	int ratioNew;
	int ratioOld = 100;
	int checkPoint = 100;

	OPERATION_TYPE = COMPRESS_OPERATION;
	rawData->resetSeeker();
	compressedData->resetSeeker();

	hashTable = new int[TABLE_SIZE];
	codeValue = new short[TABLE_SIZE];

	int fcode;
	int i;
	int c;
	unsigned short stringCode;
	int disp;
	int hsize_reg;
	int hshift;

	initCompressParameters();

	numBits = initBits;
	maxCode = MAXVAL(numBits);
	clearCode = 1 << (numBits - 1);
	terminatorCode = clearCode + 1;
	nextCode = clearCode + 2;

	stringCode = rawData->getByte();

	hshift = 0;
	for (fcode = hashTableSize; fcode < 65536; fcode *= 2)
		++hshift;
	hshift = 8 - hshift;            // set hash code range bound
	hsize_reg = hashTableSize;

	// clear the hash table
	clearHashTable();

	// start compressing operation
	event = new LZWCompressionEvent(OPERATION_STARTED);
	fireEvent(event);
	delete event;

	// output the clear code
	outputCode(clearCode);

	outer_loop:
	while ((c = rawData->getByte()) != -1) // end not reached
	{
		// fire pacifying event
		if (!((++bytesIn) % PACIFIER))
		{
			event = new LZWCompressionEvent(PERIODIC_COMPRESSION);
			fireEvent(event);
			delete event;
		}

		fcode = (c << maxBits) + stringCode;
		i = (c << hshift) ^ stringCode;      // xor hashing
		
		if (hashTable[i] == fcode)
		{
			stringCode = codeValue[i];
			continue;
		}
		else if (hashTable[i] >= 0)    // non-empty slot
		{
			disp = hsize_reg - i;   // secondary hash (after G. Knott)
			if (i == 0)
				disp = 1;
			do
			{
				if ((i -= disp) < 0)
					i += hsize_reg;
				if (hashTable[i] == fcode)
				{
					stringCode = codeValue[i];
					goto outer_loop;
				}
			}
			while (hashTable[i] >= 0);
		}

		outputCode(stringCode);
		stringCode = c;

		if (nextCode < upperBoundCode)
		{
			codeValue[i] = nextCode++;    // code -> hashtable
			hashTable[i] = fcode;
		}
		else
		{
			/*
			if (bytesIn > checkPoint)
			{
				ratioNew = (bytesOut * 100)/bytesIn;

				if (ratioNew > ratioOld)
				{
					ratioOld = 100;
			*/
					// old code
					clearHashTable();
					nextCode = clearCode + 2;
					clearFlag = true;
					outputCode(clearCode);
			/*		// old code
				}
				else
				{
					ratioOld = ratioNew;
				}

				checkPoint = bytesIn + 100;
			}
			*/
		}
	}

	// output the final code
	outputCode(stringCode);
	outputCode(terminatorCode);

	event = new LZWCompressionEvent(OPERATION_TERMINATED);
	fireEvent(event);
	delete event;


	delete [] hashTable;
	delete [] codeValue;
}

void LZWCompressor::compress2()
{
	// allocate the needed resources
	codeValue = new short[TABLE_SIZE];
	prefixCode = new unsigned short[TABLE_SIZE];
	appendCharacter = new byte[TABLE_SIZE];

	OPERATION_TYPE = COMPRESS_OPERATION;

	// reset the seeker of the raw data buffer
	rawData->resetSeeker();
	// and the same for the compressed data buffer
	compressedData->resetSeeker();

	bytesIn = bytesOut = 0;

	init();

	checkpoint = CHECK_TIME;

	nextCode = firstCode;
	
	unsigned short character;

	unsigned short stringCode;

	unsigned short index;

	int readedByte;

	int ratio_new;
	int ratio_old = 100;

	for (int i = 0; i < TABLE_SIZE; i++)
	{
		codeValue[i] = -1;
	}

	//printf("Compressing file...\n");
	// notify listeners the operation has started
	event = new LZWCompressionEvent(OPERATION_STARTED);
	fireEvent(event);
	delete event;

	// output the clear code to begin with
	outputCode(CLEAR_TABLE);
	event = new LZWCompressionEvent(CLEARING_TABLE);
	fireEvent(event);
	delete event;

	//string_code = (byte) getc(input); // get the first code
	stringCode = rawData->getByte();

	// main loop
	while ((readedByte = rawData->getByte()) != -1)
	{
		character = (unsigned short) readedByte;
		// for printing purposes each 1000 bytes print an *
		if ((++bytesIn) % 1000 == 0)
		{
			// send the pacifier to listeners
			event = new LZWCompressionEvent(PERIODIC_COMPRESSION);
			fireEvent(event);
			delete event;
		}

		// if string + character is in the table
		// find the index using the hash function
		index = findIndexFromHashtable(stringCode, character);

		// if the table contains a valid code
		if (codeValue[index] != -1)
		{
			stringCode = codeValue[index];
		}
		else
		{
			// must not overflow - here is the cause of the inefficiency
			if (nextCode <= maxCode)
			{
				codeValue[index] = nextCode ++;
				prefixCode[index] = stringCode;
				appendCharacter[index] = (byte) character;
			}

			outputCode(stringCode);
			stringCode = character;

			// is the table full ?
			if (nextCode > maxCode)
			{
				if (numBits < BITS) // do I have any more bits? if so just increase code size
				{
					// notify listeners the bitsize has increased
					event = new LZWCompressionEvent(INCREMENTING_CODE_SIZE);
					fireEvent(event);
					delete event;

					maxCode = MAXVAL(++numBits);
				}
				else if (bytesIn > checkpoint) // at checkpoint ?
				{
					if (numBits == BITS)
					{
						// compute the new compression ratio
						
						ratio_new = (bytesOut * 100)/bytesIn;
						
						if (ratio_new > ratio_old) // has the ratio degraded ?
						{
							outputCode(CLEAR_TABLE); // output the clear code

							// just mark the cleaning of the table
							// send this to listeners
							event = new LZWCompressionEvent(CLEARING_TABLE);
							fireEvent(event);
							delete event;
							
							// reset the number of bits per code
							numBits = startNoOfBits + 1;

							nextCode = firstCode;

							maxCode = MAXVAL(numBits);

							bytesIn = bytesOut = 0;

							ratio_old = 100;

							// resetting the code value array - clear the table
							for (int i = 0; i < TABLE_SIZE; i++)
							{
								codeValue[i] = -1;
							}
						}
						else
						{
							ratio_old = ratio_new;
						}
					}
					checkpoint = bytesIn + CHECK_TIME;
				}
			}
		}
	}

	outputCode(stringCode); // put the remaining string

	if (nextCode == maxCode)
	{
		++ numBits;
		
		// notify increasing code size to every listener
		event = new LZWCompressionEvent(INCREMENTING_CODE_SIZE);
		fireEvent(event);
		delete event;
	}

	outputCode(TERMINATOR); // send the terminator

	outputCode(0); // the output buffer is a 32 bit buffer and must be flushed
	outputCode(0);
	outputCode(0);

	// trims the compressedData buffer to size
	compressedData->trimToSize();

	delete [] codeValue;
	delete [] prefixCode;
	delete [] appendCharacter;

	// notify termination to listeners
	event = new LZWCompressionEvent(OPERATION_TERMINATED);
	fireEvent(event);
	delete event;
}

/**
 * LZWCompressor::addCompressionListener
 *	The method allows LZWCompressionListener objects to register for
 *	callback events.
 */
void LZWCompressor::addCompressionListener(LZWCompressionListener* listener)
{
	listeners->addTail(listener);
}

/**
 * LZWCompressor::removeCompressionListener
 *	The method removes the listener from the listener list.
 */
void LZWCompressor::removeCompressionListener(LZWCompressionListener* listener)
{
	listeners->remove(listener);
}

/**
 * LZWCompressor::fireEvent
 *	The method notifies all the listenig objects of the event occurence.
 */
void LZWCompressor::fireEvent(LZWCompressionEvent* event)
{
	ListIterator<LZWCompressionListener*>* iterator = 
		new ListIterator<LZWCompressionListener*>(listeners);

	while (iterator->hasNext())
	{
		iterator->next()->actionPerformed(event);
	}

	delete iterator;
}

