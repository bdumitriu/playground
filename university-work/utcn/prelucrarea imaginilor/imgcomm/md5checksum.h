#ifndef MD5CHECKSUM_H
#define MD5CHECKSUM_H

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "defs.h"

/**
 * This class offers several methods for computing the MD5 checksum
 * of a file or array of bytes. The three public methods you will
 * probably be interested in as a user of this class (as opposed to
 * an extender of this class) are:<br />
 * <ul>
 * <li>static method {@link #getMD5(byte*,unsignedlong) getMD5(byte*, unsigned long)}</li>
 * <li>static method {@link #getMD5(FILE*)}</li>
 * <li>static method {@link #getMD5(constchar*) getMD5(char*)}</li>
 * </ul>
 *
 * @author Bogdan Dumitriu
 * @author email: bdumitriu@bdumitriu.ro
 * @version 0.1
 */
class MD5Checksum
{
public:
	/**
	 * Computes and returns the hexadecimal MD5 checksum for
	 * the first <code>length</code> bytes of data from the
	 * <code>buffer</code>.
	 *
	 * @param buffer pointer to memory buffer containing the bytes to be checksummed
	 * @param length the number of bytes (starting from address <code>buffer</code>) to
	 *	be checksummed
	 *
	 * @return the computed 32 hexadecimal char-long MD5 checksum
	 */
	static char* getMD5(byte* buffer, unsigned long length);

	/**
	 * Computes and returns the hexadecimal MD5 checksum for
	 * the specified file. The file should be opened in binary
	 * readonly mode before calling this method. The file is not
	 * closed by the method's code so this should be done by the
	 * user.
	 *
	 * @param file the file to be checksummed
	 *
	 * @return the computed 32 hexadecimal char-long MD5 checksum
	 *	or NULL if any error occurs while reading the file
	 */
	static char* getMD5(FILE* file);

	/**
	 * Computes and returns the hexadecimal MD5 checksum for
	 * the specified file. <code>path</code> is expected to be the full
	 * path to the file.
	 *
	 * @param path the path to file to be checksummed
	 *
	 * @return the computed 32 hexadecimal char-long MD5 checksum
	 *	or NULL if any error occurs while reading the file
	 */
	static char* getMD5(const char* path);

protected:

	/**
	 * Builds a new MD5Checksum object which can be used to compute a MD5 checksum.
	 */
	MD5Checksum();


	/**
	 * This is the destructor of the MD5Checksum class.
	 */
	virtual ~MD5Checksum() {};

	/**
	 * MD5 basic transformation algorithm. An MD5 checksum is computed
	 * by four rounds of transformation. The MD5 checksum currently held
	 * in the checksum data member is merged by the transformation process
	 * with data passed in <code>block</code>.
	 *
	 * @param block a block of 64 bytes to be computed
	 */
	void transform(byte block[64]);

	/**
	 * Computes the partial MD5 checksum for <code>length</code> bytes of
	 * data in <code>input</code>.
	 *
	 * @param input a pointer to the bytes to be checksummed
	 * @param length the number of bytes (starting from address <code>input</code>) to
	 *	be checksummed
	 */
	void update(byte* input, unsigned long length);

	/**
	 * Ends the checksum calculation and returns the final hexadecimal
	 * MD5 checksum result.
	 *
	 * @return the computed 32 hexadecimal char-long MD5 checksum
	 */
	char* final();

	/**
	 * Rotates the bits in a 32 bit unsigned long (<code>x</code>) left by a specified amount
	 * (<code>n</code>) and returns the result.
	 *
	 * @param x the unsigned long which has to be rotated
	 * @param n a number specifiying how many times the bits in <code>x</code>
	 *	have to rotated
	 *
	 * @return the original unsigned long rotated by <code>n</code> bits
	 */
	inline unsigned long rotateLeft(unsigned long x, long n);

	/** Part of the implementation of basic MD5 transformation algorithm. */
	inline void FF(unsigned long& A, unsigned long B, unsigned long C, unsigned long D, unsigned long X, unsigned long S, unsigned long T);

	/** Part of the implementation of basic MD5 transformation algorithm. */
	inline void GG(unsigned long& A, unsigned long B, unsigned long C, unsigned long D, unsigned long X, unsigned long S, unsigned long T);

	/** Part of the implementation of basic MD5 transformation algorithm. */
	inline void HH(unsigned long& A, unsigned long B, unsigned long C, unsigned long D, unsigned long X, unsigned long S, unsigned long T);

	/** Part of the implementation of basic MD5 transformation algorithm. */
	inline void II(unsigned long& A, unsigned long B, unsigned long C, unsigned long D, unsigned long X, unsigned long S, unsigned long T);

	/**
	 * Transfers the data from a 32 bit array to a 8 bit array. <code>output</code>
	 * represents the 8 bit destination array, <code>input</code> represents the 32 bit
	 * source array and <code>length</code> represents the number of 8 bit data
	 * items in the destination array.
	 * <br /><br />
	 * One unsigned long from the input array is transferred into four bytes
	 * in the output array. The first (0-7) bits of the first unsigned long are 
	 * transferred to the first output byte, bits 8-15 are transferred to
	 * the second byte, etc. 
	 * <br /><br />
	 * The algorithm assumes that the output array is a multiple of 4 bytes long
	 * so that there is a perfect fit of the 32 bit unsigned long's into the 8 bit bytes.
	 *
	 * @param output the 8 bit destination array
	 * @param input the 32 bit source array
	 * @param length the number of 8 bit data items in the destination array
	 */
	inline void ulongToByte(byte* output, unsigned long* input, unsigned long length);

	/**
	 * Transfers the data from an 8 bit array to a 32 bit array. <code>output</code>
	 * represents the 32 bit destination array, <code>input</code> represents the 8 bit
	 * source array and <code>length</code> represents the number of 8 bit data
	 * items in the source array.
	 * <br /><br />
	 * Four bytes from the input array are transferred to each unsigned long entry
	 * of the output array. The first byte is transferred to the bits (0-7)
	 * of the output unsigned long, the second byte to bits 8-15, etc.
	 * <br /><br />
	 * The algorithm assumes that the input array is a multiple of 4 bytes long
	 * so that there is a perfect fit into the array of 32 bit words.
	 *
	 * @param output the 32 bit destination array
	 * @param input the 8 bit source array
	 * @param length the number of 8 bit data items in the source array
	 */
	inline void byteToUlong(unsigned long* output, byte* input, unsigned long length);

private:
	/** input buffer */
	byte buffer[64];

	/** number of *bits* in input buffer modulo 2^64 (bitCount[0] = lsb) */
	unsigned long bitCount[2];

	/** MD5 checksum */
	unsigned long checksum[4];
};

#endif // MD5CHECKSUM_H
