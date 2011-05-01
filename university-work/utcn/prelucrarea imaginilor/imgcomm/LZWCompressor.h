

#ifndef _LZWCOMPRESSOR_H_
#define _LZWCOMPRESSOR_H_

#define PACIFIER 1000

#include "debug.h"

#include "ByteBuffer.h"
#include "LZWCompressionListener.h"
#include "AbstractList.h"

/** The size of the byte.
*/
#define BYTE_SIZE 8

/** The maximum number of bits the code can have.
*/
#define BITS 12

/** The ammount of bits the code starts with.
*/
#define INIT_BITS 9

/** The number of bits used by the hash function.
*/
#define HASHING_SHIFT_BITS BITS - BYTE_SIZE

/** 
* This is a macro used to compute the maximum value of a number represented
* on <code>n</code> bits.
*/
#define MAXVAL(n) ((1 << (n)) - 1)

/**
* The size of the hash table - must be 25% larger than the number of possible
* elements that is 2 ^ 12 in this case, and also must be a prime number because
* the hash table is a closed one.
*/
#define TABLE_SIZE 5021

/** The size of the decode stack - used to backtrace into the decoder.
*/
#define STACK_SIZE 5021

/** The clear code.
*/
#define CLEAR_TABLE 256

/** The terminator code.
*/
#define TERMINATOR 257

/** The first available code.
*/
#define FIRST_CODE 258

/** The time checking interval - for the compression ratio monitorisation.
*/
#define CHECK_TIME 100

/** The type of the operation performed.
*/
#define COMPRESS_OPERATION 0
#define EXPAND_OPERATION 1


/**
* The mask used to filter just the needed bytes out of the full 32 bit
* register.
*/
static long code_mask[17] = 
{
	 0,
	0x0001, 0x0003,
	0x0007, 0x000F,
	0x001F, 0x003F,
	0x007F, 0x00FF,
	0x01FF, 0x03FF,
	0x07FF, 0x0FFF,
	0x1FFF, 0x3FFF, 
	0x7FFF, 0xFFFF
};

/** The policy used to handle memory buffers.
*/
enum DeletePolicy
{
	doDelete = true,
	doNotDelete = false,
};


/**
 * The class responsible for the LZW compression.
 * It is made to support the gif version of LZW, with code size
 * from 9 up to 12 bits, and using a clear table. <br /><br />
 *
 * Author Tudor Marian <br />
 *
 * Technical University Of Cluj-Napoca <br />
 * Computer Science Departament <br />
 * gr. 3241/1<br />
 *
 * created: april, 13, 2003<br />
 * last modified: april 19, 2003<br />
 *
 * @author	Tudor Marian
 */
class LZWCompressor
{
public:

	/**
	* The empty constructor.
	*/
	LZWCompressor();

	/**
	* Constructs an object & sets the raw data or the compressed data stream
	* according to the <code>operation</code> parameter.
	*
	* @param	operation	the type of the operation used.
	* param		data	the buffer containing the data.
	*/
	LZWCompressor(int operation, ByteBuffer* data);

	/**
	* Constructs a compressor object with both the compressed data stream and
	* the raw data stream.
	*
	* @param	rawData	the raw data stream.
	* @param	compressedData	the compressed data buffer.
	*/
	LZWCompressor(ByteBuffer* rawData, ByteBuffer* compressedData);
	
	/**
	* Destroys the object.
	*/
	~LZWCompressor();

	/**
	* The method will compress the data given to the object into the
	* <code>rawData</code> buffer, and will store it within the own 
	* created <code>compressedData</code> buffer.
	*/
	void compress();

	void compress2();
					
	/**
	* The method expands the data found within the <code>compressedData</code>
	* buffer, and stores it within the <code>rawData</code> buffer.
	*/
	void expand(); 

	void expand2();

	/**
	* Returns the raw data stream.
	*
	* @return	the raw data buffer.
	*/
	ByteBuffer* getRawData() {return rawData;};

	/**
	* Sets the raw data stream.
	*
	* @param	rawData	the raw data stream.
	*/
	void setRawData(ByteBuffer* rawData);

	/**
	* Returns the compressed data stream.
	*
	* @return	the compressed data stream.
	*/
	ByteBuffer* getCompressedData() {return compressedData;};

	/**
	* Sets the compressed data stream.
	*
	* @param	compressedData	the compressed data stream.
	*/
	void setCompressedData(ByteBuffer* compressedData);

	/**
	* The metohd clears the raw data buffer.
	*/
	void clearRawData() {rawData->clear();};

	/**
	* The method clears the compressed data buffer.
	*/
	void clearCompressedData() {compressedData->clear();};

	/**
	* The method allows a LZWCompressionListener object to register itself
	* for any triggered events.
	*
	* @param	listener	the object to register itself for event notification.
	*/
	void addCompressionListener(LZWCompressionListener* listener);

	/**
	* Removes the LZWCompressionListener object from the list of listeners.
	*
	* @param	listener	the object to be removed from the notification list.
	*/
	void removeCompressionListener(LZWCompressionListener* listener);

	/**
	* Sets the number of bits per code to start with during the LZW compression/
	* decompression.
	*
	* @param	numBits	the mumber of bits to start with for the code.
	*/
	void setStartNoOfBits(int numBits);

	/**
	* Consideres the policy for deletion of the compressed data.
	*
	* @param	policy	the policy used during the compressed data manipulation.
	*/
	void setCompressedDataDeletePolicy(DeletePolicy policy);

	/**
	* Consideres the policy for deletion of the raw data.
	*
	* @param	policy	the policy used during the raw data manipulation.
	*/
	void setRawDataDeletePolicy(DeletePolicy policy);

	/**
	* Set policies both for the raw data and for the compressed data to
	* to be deleted.
	*/
	void setDefaultPolicies();

protected:

	/**
	* The metohd is called when an LZWCompressionEvent object is created
	* and the actionPerformed method is to be called for notifying all
	* listening objects of the event occurence.
	*
	* @param	event	the event to be sent out to every registered listener.
	*/
	void fireEvent(LZWCompressionEvent* event);

private:
	/** The raw data buffer.
	*/
	ByteBuffer* rawData;

	/** The delete policy for the raw data buffer.
	*/
	DeletePolicy rawDataDeletePolicy;

	/** The compressed data  buffer.
	*/
	ByteBuffer* compressedData;
	
	/** The delete policy for the compressed data buffer.
	*/
	DeletePolicy compressedDataDeletePolicy;

	/**
	* The following is the table of codes - it also is given as a closed
	* hash table.
	*/
	short* codeValue;

	/**
	* The hash table used to perform the encoding algorithm.
	*/
	int* hashTable;

	/**
	* The hash table size
	*/
	int hashTableSize;

	/** The number of bits to start with.
	*/
	int initBits;

	/**
	* The following is the table of prefix code, the string as named in the LZW
	* algorithm.
	*/
	unsigned short* prefixCode;

	/**
	* The following is the array holding the append character
	*/
	byte* appendCharacter;

	/** The decode stack.
	*/
	byte* decodeStack;

	/**
	* The number of bytes outputted, required parameter for compression
	* ratio monitorisation.
	*/
	int bytesOut;
	int bytesIn;

	/** The number of current bits for the compression code.
	*/
	int numBits;

	/** The upper bounds of the code value.
	*/
	int maxCode;
	int upperBoundCode;

	/** The max number of bits.
	*/
	int maxBits;

	/** The checkpoint marker for monitoring the compression ratio.
	*/
	int checkpoint;

	/**
	* The following is a parameter handling the type of operation performed
	* this operation can be compress or expand.
	*/
	byte OPERATION_TYPE;

	/**
	* The list of LZWCompressionListener objects held for callback purposes.
	*/
	List<LZWCompressionListener*>* listeners;

	/**
	* The method returns the hash code - meaning an index computed
	* with the values of its two parameters. The operations are made
	* on a closed hash table.
	*
	* @param	hashPrefix	the prefix code used to compute the hash function.
	* @param	hashCharacter	the character used to compute the hash function.
	* @return	the index in the hash table.
	*/
	unsigned short findIndexFromHashtable(unsigned short hashPrefix, 
		unsigned short hashCharacter);

	/**
	* The method outputs a code generated by the compression algortihm, 
	* warning: the code is stored into the <code>compressedData</code> buffer.
	*
	* @param	code	the code to be outputted.
	*/
	void outputCode(unsigned short code);

	/**
	* The method gets a code from the <code>compressedData</code> buffer.
	*
	* @return	the code read.
	*/
	unsigned short inputCode();

	/**
	* The method decodes a string, given the stack of codes & the current code.
	*
	* @param	buffer	the decode stack containing the codes.
	* @param	code	the code to start with the decoding process.
	* @return	the processed stack.
	*/
	byte* decodeString(byte* buffer, unsigned short code);

	/** The number of bits the decoder starts with.
	*/
	int startNoOfBits;

	/**
	* The method initializes the parameters.
	*/
	bool init();

	/**
	* The method does the initializations for the compression algorithm.
	*/
	void initCompressParameters();

	/** The clear code.
	*/
	unsigned short clearCode;

	/** The terminator code.
	*/
	unsigned short terminatorCode;

	/** The first code.
	*/
	unsigned short firstCode;

	/** The next code.
	*/
	unsigned short nextCode;

	/** The following is a register-size integer used to output code.
	*/
	unsigned long outputBitBuffer;

	/** The number of bits it holds up at a certain moment of time.
	*/
	int bitCount;

	/** If true a clear operation is pending.
	*/
	bool clearFlag;

	/** Compression event object.
	*/
	LZWCompressionEvent* event;

	/**
	* Clears the hash table.
	*/
	void clearHashTable();

	/**
	* Count element for the input code method.
	*/
	int noOfBitsLeft;

	/**
	* The read byte by the input code method.
	*/
	byte octet;
};

#endif //_LZWCOMPRESSOR_H_


