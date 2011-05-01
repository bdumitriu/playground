#include "md5checksum.h"
#include "md5checksumdefines.h"

char* MD5Checksum::getMD5(const char* path)
{
	/* open the file as a binary file in readonly mode */
	FILE* file;
	if ((file = fopen(path, "rb")) == NULL)
		return NULL;

	/* the file has been successfully opened, */
	/* so now get and return its checksum */
	return getMD5(file);
}

char* MD5Checksum::getMD5(FILE* file)
{
	MD5Checksum md5Checksum;
	long bytesRead = 0;
	const long bufferSize = 1024;	// checksum the file in blocks of 1024 bytes
	byte buffer[bufferSize];	// buffer for data read from the file

	while ((bytesRead = fread(&buffer[0], 1, bufferSize, file)) > 0)
	{
		md5Checksum.update(buffer, bytesRead);
	}

	/* finalise the checksum and return it */
	return md5Checksum.final();
}

char* MD5Checksum::getMD5(byte* buffer, unsigned long length)
{
	/* compute and return the checksum */
	MD5Checksum md5c;

	md5c.update(buffer, length);
	return md5c.final();
}

unsigned long MD5Checksum::rotateLeft(unsigned long x, long n)
{
	/* rotate and return x */
	return (x << n) | (x >> (32-n));
}

void MD5Checksum::FF(unsigned long& A, unsigned long B, unsigned long C, unsigned long D,
	unsigned long X, unsigned long S, unsigned long T)
{
	unsigned long F = (B & C) | (~B & D);
	A += F + X + T;
	A = rotateLeft(A, S);
	A += B;
}

void MD5Checksum::GG(unsigned long& A, unsigned long B, unsigned long C, unsigned long D,
	unsigned long X, unsigned long S, unsigned long T)
{
	unsigned long G = (B & D) | (C & ~D);
	A += G + X + T;
	A = rotateLeft(A, S);
	A += B;
}

void MD5Checksum::HH(unsigned long& A, unsigned long B, unsigned long C, unsigned long D,
	unsigned long X, unsigned long S, unsigned long T)
{
	unsigned long H = (B ^ C ^ D);
	A += H + X + T;
	A = rotateLeft(A, S);
	A += B;
}

void MD5Checksum::II(unsigned long& A, unsigned long B, unsigned long C, unsigned long D,
	unsigned long X, unsigned long S, unsigned long T)
{
	unsigned long I = (C ^ (B | ~D));
	A += I + X + T;
	A = rotateLeft(A, S);
	A += B;
}

void MD5Checksum::byteToUlong(unsigned long* output, byte* input, unsigned long length)
{
	/* initializations */
	unsigned long i = 0;	// index to output array
	unsigned long j = 0;	// index to input array

	/* transfer the data by shifting and copying */
	for ( ; j < length; i++, j += 4)
	{
		output[i] = (unsigned long) input[j] |
			(unsigned long) input[j+1] << 8	|
			(unsigned long) input[j+2] << 16 |
			(unsigned long) input[j+3] << 24;
	}
}

void MD5Checksum::transform(byte block[64])
{
	/* initialise local data with current checksum */
	unsigned long a = checksum[0];
	unsigned long b = checksum[1];
	unsigned long c = checksum[2];
	unsigned long d = checksum[3];

	/* copy bytes from input <block> to an array of unsigned longs 'X' */
	unsigned long X[16];
	byteToUlong(X, block, 64);

	/* perform Round 1 of the transformation */
	FF(a, b, c, d, X[ 0], MD5_S11, MD5_T01);
	FF(d, a, b, c, X[ 1], MD5_S12, MD5_T02);
	FF(c, d, a, b, X[ 2], MD5_S13, MD5_T03);
	FF(b, c, d, a, X[ 3], MD5_S14, MD5_T04);
	FF(a, b, c, d, X[ 4], MD5_S11, MD5_T05);
	FF(d, a, b, c, X[ 5], MD5_S12, MD5_T06);
	FF(c, d, a, b, X[ 6], MD5_S13, MD5_T07);
	FF(b, c, d, a, X[ 7], MD5_S14, MD5_T08);
	FF(a, b, c, d, X[ 8], MD5_S11, MD5_T09);
	FF(d, a, b, c, X[ 9], MD5_S12, MD5_T10);
	FF(c, d, a, b, X[10], MD5_S13, MD5_T11);
	FF(b, c, d, a, X[11], MD5_S14, MD5_T12);
	FF(a, b, c, d, X[12], MD5_S11, MD5_T13);
	FF(d, a, b, c, X[13], MD5_S12, MD5_T14);
	FF(c, d, a, b, X[14], MD5_S13, MD5_T15);
	FF(b, c, d, a, X[15], MD5_S14, MD5_T16);

	/* perform Round 2 of the transformation */
	GG(a, b, c, d, X[ 1], MD5_S21, MD5_T17);
	GG(d, a, b, c, X[ 6], MD5_S22, MD5_T18);
	GG(c, d, a, b, X[11], MD5_S23, MD5_T19);
	GG(b, c, d, a, X[ 0], MD5_S24, MD5_T20);
	GG(a, b, c, d, X[ 5], MD5_S21, MD5_T21);
	GG(d, a, b, c, X[10], MD5_S22, MD5_T22);
	GG(c, d, a, b, X[15], MD5_S23, MD5_T23);
	GG(b, c, d, a, X[ 4], MD5_S24, MD5_T24);
	GG(a, b, c, d, X[ 9], MD5_S21, MD5_T25);
	GG(d, a, b, c, X[14], MD5_S22, MD5_T26);
	GG(c, d, a, b, X[ 3], MD5_S23, MD5_T27);
	GG(b, c, d, a, X[ 8], MD5_S24, MD5_T28);
	GG(a, b, c, d, X[13], MD5_S21, MD5_T29);
	GG(d, a, b, c, X[ 2], MD5_S22, MD5_T30);
	GG(c, d, a, b, X[ 7], MD5_S23, MD5_T31);
	GG(b, c, d, a, X[12], MD5_S24, MD5_T32);

	/* perform Round 3 of the transformation */
	HH(a, b, c, d, X[ 5], MD5_S31, MD5_T33);
	HH(d, a, b, c, X[ 8], MD5_S32, MD5_T34);
	HH(c, d, a, b, X[11], MD5_S33, MD5_T35);
	HH(b, c, d, a, X[14], MD5_S34, MD5_T36);
	HH(a, b, c, d, X[ 1], MD5_S31, MD5_T37);
	HH(d, a, b, c, X[ 4], MD5_S32, MD5_T38);
	HH(c, d, a, b, X[ 7], MD5_S33, MD5_T39);
	HH(b, c, d, a, X[10], MD5_S34, MD5_T40);
	HH(a, b, c, d, X[13], MD5_S31, MD5_T41);
	HH(d, a, b, c, X[ 0], MD5_S32, MD5_T42);
	HH(c, d, a, b, X[ 3], MD5_S33, MD5_T43);
	HH(b, c, d, a, X[ 6], MD5_S34, MD5_T44);
	HH(a, b, c, d, X[ 9], MD5_S31, MD5_T45);
	HH(d, a, b, c, X[12], MD5_S32, MD5_T46);
	HH(c, d, a, b, X[15], MD5_S33, MD5_T47);
	HH(b, c, d, a, X[ 2], MD5_S34, MD5_T48);

	/* perform Round 4 of the transformation */
	II(a, b, c, d, X[ 0], MD5_S41, MD5_T49);
	II(d, a, b, c, X[ 7], MD5_S42, MD5_T50);
	II(c, d, a, b, X[14], MD5_S43, MD5_T51);
	II(b, c, d, a, X[ 5], MD5_S44, MD5_T52);
	II(a, b, c, d, X[12], MD5_S41, MD5_T53);
	II(d, a, b, c, X[ 3], MD5_S42, MD5_T54);
	II(c, d, a, b, X[10], MD5_S43, MD5_T55);
	II(b, c, d, a, X[ 1], MD5_S44, MD5_T56);
	II(a, b, c, d, X[ 8], MD5_S41, MD5_T57);
	II(d, a, b, c, X[15], MD5_S42, MD5_T58);
	II(c, d, a, b, X[ 6], MD5_S43, MD5_T59);
	II(b, c, d, a, X[13], MD5_S44, MD5_T60);
	II(a, b, c, d, X[ 4], MD5_S41, MD5_T61);
	II(d, a, b, c, X[11], MD5_S42, MD5_T62);
	II(c, d, a, b, X[ 2], MD5_S43, MD5_T63);
	II(b, c, d, a, X[ 9], MD5_S44, MD5_T64);

	/* add the transformed values to the current checksum */
	checksum[0] += a;
	checksum[1] += b;
	checksum[2] += c;
	checksum[3] += d;
}

MD5Checksum::MD5Checksum()
{
	/* zero members */
	memset(buffer, 0, 64);
	bitCount[0] = bitCount[1] = 0;

	/* initialize md5 checksum with the magic constants */
	checksum[0] = MD5_INIT_STATE_0;
	checksum[1] = MD5_INIT_STATE_1;
	checksum[2] = MD5_INIT_STATE_2;
	checksum[3] = MD5_INIT_STATE_3;
}

void MD5Checksum::ulongToByte(byte* output, unsigned long* input, unsigned long length)
{
	/* transfer the data by shifting and copying */
	unsigned long i = 0;
	unsigned long j = 0;
	for ( ; j < length; i++, j += 4)
	{
		output[j] = (byte) (input[i] & 0xff);
		output[j+1] = (byte) ((input[i] >> 8) & 0xff);
		output[j+2] = (byte) ((input[i] >> 16) & 0xff);
		output[j+3] = (byte) ((input[i] >> 24) & 0xff);
	}
}

char* MD5Checksum::final()
{
	/* save number of bits */
	byte bits[8];
	ulongToByte(bits, bitCount, 8);

	/* pad out to 56 mod 64 */
	unsigned long idx = (unsigned long) ((bitCount[0] >> 3) & 0x3f);
	unsigned long padLength = (idx < 56) ? (56 - idx) : (120 - idx);
	update(PADDING, padLength);

	/* append length (before padding) */
	update(bits, 8);

	/* store final state in <md5> */
	const long md5Size = 16;
	unsigned char md5[md5Size];
	ulongToByte(md5, checksum, md5Size);

	/* convert the hexadecimal checksum to a char* */
	char* strMD5 = (char*) malloc(sizeof(char) * md5Size * 2);
	if (strMD5 == NULL)
	{
		printf("Memory allocation error.\n");
		exit(1);
	}

	idx = 0;
	for (int i = 0; i < md5Size; i++)
	{
		if (md5[i] <= 9)
		{
			strMD5[idx++] = '0';
			strMD5[idx++] = md5[i] + '0';
		}
		else if (md5[i] <= 15)
		{
			strMD5[idx++] = '0';
			strMD5[idx++] = (md5[i] - 10) + 'a';
		}
		else
		{
			int first = (int) md5[i] / 16;
			int second = md5[i] % 16;
			if (first <= 9)
				strMD5[idx++] = first + '0';
			else
				strMD5[idx++] = (first - 10) + 'a';
			if (second <= 9)
				strMD5[idx++] = second + '0';
			else
				strMD5[idx++] = (second - 10) + 'a';
		}
	}

	return strMD5;
}

void MD5Checksum::update(byte* input, unsigned long length)
{
	/* compute number of bytes mod 64 */
	unsigned long idx = (unsigned long) ((bitCount[0] >> 3) & 0x3F);

	/* update number of bits */
	if ((bitCount[0] += (length << 3)) < (length << 3))
	{
		bitCount[1]++;
	}
	bitCount[1] += (length >> 29);

	/* transform as many times as possible */
	unsigned long i = 0;
	unsigned long partLength = 64 - idx;
	if (length >= partLength)
	{
		memcpy(&buffer[idx], input, partLength);
		transform(buffer);
		for (i = partLength; i + 63 < length; i += 64)
		{
			transform(&input[i]);
		}
		idx = 0;
	}
	else 
	{
		i = 0;
	}

	/* buffer remaining input */
	memcpy(&buffer[idx], &input[i], length-i);
}
