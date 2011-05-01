
#include "Bmp2Gif.h"

#include "math.h"

Bmp2Gif::Bmp2Gif()
{
}

Bmp2Gif::~Bmp2Gif()
{
}

ByteBuffer* Bmp2Gif::serializeBmp2Gif(BmpImage* bmp)
{
	// compute the aprox size of the gif ?

	ByteBuffer* gif = new ByteBuffer();

	// add up the signature GIF
	gif->addRGB(GIF_SIGNATURE);

	// add up the version 98a
	gif->addRGB(gif89a);

	// add logical screen width
	word width = (word) bmp->getWidth();
	
	gif->addWord(width);

	// add logical screen height
	word height = (word) bmp->getHeight();

	gif->addWord((word) height);

	// add up the packed fields
	byte bitsPerPixel = (byte) bmp->noOfBitsPerPixel();
	ASSERT(bitsPerPixel <= 8); // else must do the nasty thing
	
	register byte buffer = 0x80; // thus setting the global color table flag and the sort flag

	// add up the color resolution
	buffer |= (bitsPerPixel - 1) << 4; 
	
	// add up the size of the global color table
	int colorTableSize = bmp->numberOfColors();

	ASSERT(colorTableSize == (1 << bitsPerPixel));

	buffer |= (bitsPerPixel-1);

	gif->addByte(buffer);

	// add the background color index
	buffer = 0x00;

	gif->addByte(buffer);

	// add pixel aspect ratio
	gif->addByte(buffer);

	// global color table
	for (int i = 0; i < colorTableSize; i++)
	{
		// add the red
		buffer = (byte) (bmp->getColors()[i] >> 16);
		gif->addByte(buffer);

		buffer = (byte) (bmp->getColors()[i] >> 8);
		gif->addByte(buffer);

		buffer = (byte) (bmp->getColors()[i]);
		gif->addByte(buffer);
	}

	// the image descriptor
	gif->addByte(ImageSeparator);

	// add image left & top position
	buffer = 0x00;
	gif->addWord(buffer);
	gif->addWord(buffer);

	// add image width & height
	gif->addWord(width);
	gif->addWord(height);

	// add the packed fields meaning no local color table, image not interlaced
	// no sorting, reserved bits to 0x00
	gif->addByte(buffer);

	// add up the lzw minimum code size = 0x08
	gif->addByte(0x08);

	// process the data: compress & split into blocks

	// flip image & lose the padding
	ByteBuffer* gifBitmap = new ByteBuffer(height*width);
	byte* bmpBitmap = bmp->getBitmap();

	int w = rowBytes(width * bitsPerPixel);

	for (i = 0; i < height; i++)
	{
		for (int j = 0; j < width; j++)
		{
			gifBitmap->addByte(bmpBitmap[(height-i-1)*w + j]);
		}
	}
	
	gifBitmap->resetSeeker();

	// compress data
	LZWCompressor* lzw = new LZWCompressor(COMPRESS_OPERATION, gifBitmap);
	lzw->compress();

	ByteBuffer* compressedGifBitmap = lzw->getCompressedData();

	compressedGifBitmap->trimToSize();
	compressedGifBitmap->resetSeeker();
	printf("Compressed gif bitmap: %d\n", compressedGifBitmap->getLength());

	// and pack it into 255-byte blocks
	int blockSize = 255;

	int noOfBlocks = compressedGifBitmap->getLength() / blockSize;
	
	for (i = 0; i < noOfBlocks; i++)
	{
		// add up the block size
		gif->addByte(blockSize);

		// add the compressed data bytes
		for (int j = 0; j < blockSize; j++)
		{
			gif->addByte(compressedGifBitmap->getByte());
		}
	}

	// if there are any left overs ... do the math
	blockSize = compressedGifBitmap->getLength() % blockSize;

	if (blockSize != 0)
	{
		// add the block size
		gif->addByte(blockSize);

		// and the rest of the data bytes
		for (i = 0; i < blockSize; i++)
		{
			gif->addByte(compressedGifBitmap->getByte());
		}
	}

	// add up the empty block at the end
	buffer = 0x00;
	gif->addByte(buffer);

	// add the trailer
	gif->addByte(GifTrailer);

	gif->trimToSize();
	gif->resetSeeker();
	
	// deallocate the other stuff
	delete bmp;
	delete lzw;

	return gif;
}

ByteBuffer* Bmp2Gif::serializeBmp2Gif(ByteBuffer* bmpImage)
{
	BmpImage* bmp = new BmpImage(bmpImage);
	if (!bmp->init())
	{
		printf("Source image not BMP\n");
		return NULL;
	}

	return serializeBmp2Gif(bmp);
}


BmpImage* Bmp2Gif::convertGif2Bmp(ByteBuffer* gifImage)
{
	GifImage* gif = new GifImage(gifImage);
	if (!gif->init())
	{
		printf("Source image not gif\n"); // exception should be thrown
		return NULL;
	}


	// should be implemented more efficiently, but because no setter methods
	// for any GifImage or BmpImage were implemented this will do

	ByteBuffer* bmpBuffer = serializeGif2Bmp(gifImage);
	if (bmpBuffer == NULL)
		return NULL;

	BmpImage* bmp = new BmpImage(bmpBuffer);
	bmp->init();

	return bmp;	
}

int Bmp2Gif::rowBytes(int numBits)
{
	return ((numBits + 31)/32)*4;
}

ByteBuffer* Bmp2Gif::serializeGif2Bmp(GifImage* gif)
{
		// calculus elements
	int colorTableSize = (gif->getGlobalColorTableSize()) 
		? gif->getGlobalColorTableSize() : gif->getLocalColorTableSize();
	
	gif_rgb* colorTable = (gif->getGlobalColorTableSize())
		? gif->getGlobalColorTable() : gif->getLocalColorTable();

	long bitmapOffset = BMPFILEHDRSIZE + BMPNEWWINHDRSIZE + 
		(colorTableSize * BMPNEWPALETTESIZE);

	unsigned long width = gif->getImageWidth();
	unsigned long height = gif->getImageHeight();

	// suppose to compute the size of the bitmap in bytes
	int w = rowBytes(gif->getColorResolution() * width);
	long bitmapSize = w * height;

	unsigned long fileSize = bitmapOffset + bitmapSize;

	ByteBuffer* bmpData = new ByteBuffer(fileSize);
	
	// add the file type - must be BM
	bmpData->addWord(BMPBITMAP);

	bmpData->addDword(fileSize);

	// write the reserved 2 words
	bmpData->addWord(0);
	bmpData->addWord(0);

	// the bitmap offset

	bmpData->addDword(bitmapOffset);

	// write the header size
	bmpData->addDword(BMPNEWWINHDRSIZE);

	// add image width and height in pixels
	bmpData->addDword(width);
	bmpData->addDword(height);

	// one plane
	bmpData->addWord(1);
	// no of bits per pixel
	word bitsPerPixel = gif->getColorResolution();
	bmpData->addWord(bitsPerPixel);
	// compression method - no compression 
	bmpData->addDword(0);
	
	// add bitmap size
	bmpData->addDword(bitmapSize);

	// add horizontal & vertical resolution
	bmpData->addDword(0);
	bmpData->addDword(0);

	// compute the number of colors used
	long noOfColors = 1 << bitsPerPixel;
	bmpData->addDword(noOfColors);

	// add the number of colors important
	bmpData->addDword(noOfColors);

	// add up the color palette
	ASSERT(noOfColors == colorTableSize);
	byte padding = 0x0;

	for (int k = 0; k < colorTableSize; k++)
	{
		// in a bitmap the RGB' s are reversed to be read back into little endian
		bmpData->addByte(colorTable[k].blue);
		bmpData->addByte(colorTable[k].green);
		bmpData->addByte(colorTable[k].red);
		bmpData->addByte(padding);
	}

	// now start making fun stuff

	ByteBuffer* gifBitmap = gif->decompressImageData();
	
	ASSERT(gifBitmap->getLength() == height * width);

	byte* gifArray = gifBitmap->getBytes();
	//byte* bmpArray = new byte[bitmapSize];

	// must align it if it is interlaces
	bmpData->seek(bitmapOffset);

	if (gif->isInterlaced())
	{
		// should handle the interlaced gif file
		int offset;
		dword i;

		// every 8-th row, starting with row 0
		offset = height/8;
		for (i = 0; i < height/8; i++)
		{
			// seek to the right location
			bmpData->seek(bitmapOffset + i*8*w);

			for (dword j = 0; j < width; j++)
			{
				bmpData->addByte(gifArray[(offset-i)*width + j]);

				// if I reached the end of the line add padding bytes
				for (int k = width; k < w; k++)
				{
					bmpData->addByte(padding);
				}
			}
		}

		offset = height/4;
		// every 8-th row, starting with row 4
		for (i = 0; i < height/8; i++)
		{
			// seek to correct location
			bmpData->seek(bitmapOffset + (i*8 + 4)*w);

			for (dword j = 0; j < width; j++)
			{
				bmpData->addByte(gifArray[(offset-i)*width + j]);

				// if I reached the end of the line add padding bytes
				for (int k = width; k < w; k++)
				{
					bmpData->addByte(padding);
				}
			}
		}

		offset = height/2;
		// every 4-th row, starting with row 2
		for (i = 0; i < height/4; i++)
		{
			// seek to correct location
			bmpData->seek(bitmapOffset + (i*4+2)*w);

			for (dword j = 0; j < width; j++)
			{
				bmpData->addByte(gifArray[(offset-i)*width + j]);

				// if I reached the end of the line add padding bytes
				for (int k = width; k < w; k++)
				{
					bmpData->addByte(padding);
				}
			}
		}

		offset = height;
		// every 2nd row, starting with row 1
		for (i = 0; i < height/2; i++)
		{
			// seek to correct location
			bmpData->seek(bitmapOffset + (i*2+1)*w);

			for (dword j = 0; j < width; j++)
			{
				bmpData->addByte(gifArray[(offset-i)*width + j]);

				// if I reached the end of the line add padding bytes
				for (int k = width; k < w; k++)
				{
					bmpData->addByte(padding);
				}
			}
		}
	}
	else
	{
		for (dword i = 0; i < height; i++)
		{
			for (dword j = 0; j < width; j++)
			{
				//bmpArray[i*w+j] = gifArray[(height-i)*width + j];
				bmpData->addByte(gifArray[(height-i-1)*width + j]);

				// if I reached the end of the line add padding bytes
				for (int k = width; k < w; k++)
				{
					//bmpArray[i*w+k] = padding;
					bmpData->addByte(padding);
				}
			}
		}
	}

	delete gif;

	bmpData->resetSeeker();
	return bmpData;
}

ByteBuffer* Bmp2Gif::serializeGif2Bmp(ByteBuffer* gifImage)
{
	GifImage* gif = new GifImage(gifImage);
	if (!gif->init())
	{
		printf("Source image not gif\n"); // exception should be thrown
		return NULL;
	}
	
	return this->serializeGif2Bmp(gif);
}

