/**
 *	@Author Tudor Marian
 *	Technical University of Cluj-Napoca
 *	Computer Science Departament
 *	gr. 3241/1
 *	
 *	created mai, 2nd, 2003
 */


#include "Encoder.h"

Encoder::Encoder()
{}

Encoder::~Encoder()
{}


//-------------------------------------------------------------------
// Encoder::encodeGrayscaleImage
//	Will ecode the grayscale bitmap image converting it to
//	a corespondant GIF file.
//-------------------------------------------------------------------
int Encoder::encodeGrayscaleImage(byte* bitmapImage, long bitmapSize, 
			byte** gifImage, long* gifSize)
{
	ByteBuffer* bitmapImageBuffer = new ByteBuffer(bitmapSize, bitmapImage);

	Bmp2Gif* bmp2Gif = new Bmp2Gif();
	ByteBuffer* gifImageBuffer = bmp2Gif->serializeBmp2Gif(bitmapImageBuffer);

	delete bitmapImageBuffer;
	delete bmp2Gif;

	*gifImage = gifImageBuffer->getBytes();
	*gifSize = gifImageBuffer->getLength();

	return *gifSize;
}


//-------------------------------------------------------------------
// Encoder::decodeGrayscaleImage
//	Will decode the gif representation of the image converting it
//	back into the corespondant bitmap.
//-------------------------------------------------------------------
int Encoder::decodeGrayscaleImage(byte* gifImage, long gifSize, 
			byte** bitmapImage, long* bitmapSize)
{
	ByteBuffer* gifImageBuffer = new ByteBuffer(gifSize, gifImage);

	Bmp2Gif* bmp2Gif = new Bmp2Gif();
	ByteBuffer* bmpImageBuffer = bmp2Gif->serializeGif2Bmp(gifImageBuffer);

	delete gifImageBuffer;
	delete bmp2Gif;

	*bitmapImage = bmpImageBuffer->getBytes();
	*bitmapSize = bmpImageBuffer->getLength();

	return *bitmapSize;
}

//-------------------------------------------------------------------
// Encoder::encodeHighColorImage
//	Will apply the LZW compression algorithm, thus not transforming the
//	high color image to a gif and losing color. The result is a compressed
//	LZW file that can be decompressed with the appropriate method.
//-------------------------------------------------------------------
int Encoder::encodeHighColorImage(byte* bitmapImage, long bitmapSize, 
	byte** compressedImage, long* compressedImageSize)
{
	ByteBuffer* bitmapImageBuffer = new ByteBuffer(bitmapSize, bitmapImage);
	
	LZWCompressor* lzwCompressor = new LZWCompressor(
		COMPRESS_OPERATION, bitmapImageBuffer);

	lzwCompressor->setCompressedDataDeletePolicy(doNotDelete);

	lzwCompressor->compress();

	ByteBuffer* compressedImageBuffer = lzwCompressor->getCompressedData();

	delete lzwCompressor;
	
	*compressedImage = compressedImageBuffer->getBytes();
	*compressedImageSize = compressedImageBuffer->getLength();

	return *compressedImageSize;
}


//-------------------------------------------------------------------
// Encoder::decodeHighColorImage
//	Decodes backward a LZW compressed file, here a high color bitmap
//	image.
//-------------------------------------------------------------------
int Encoder::decodeHighColorImage(byte* compressedImage, long compressedImageSize, 
			byte** bitmapImage, long* bitmapSize)
{
	ByteBuffer* compressedImageBuffer = new ByteBuffer(compressedImageSize
		, compressedImage);

	LZWCompressor* lzwCompressor = new LZWCompressor(
		EXPAND_OPERATION, compressedImageBuffer
	);

	lzwCompressor->expand();

	lzwCompressor->setRawDataDeletePolicy(doNotDelete);

	ByteBuffer* bitmapImageBuffer = lzwCompressor->getRawData();

	delete lzwCompressor;

	*bitmapImage = bitmapImageBuffer->getBytes();
	*bitmapSize = bitmapImageBuffer->getLength();

	return *bitmapSize;
}



