
#ifndef _BMP2GIF_H_
#define _BMP2GIF_H_

#include "BmpImage.h"
#include "GifImage.h"


/**
 * The class is used to convert from a bitmap into a gif and
 * viceversa. Support for converting a 24 high color bitmap image
 * is not given due to the quality lossless character of this
 * project. <br /><br />
 *
 * Author Tudor Marian <br />
 * Technical University Of Cluj-Napoca <br />
 * Computer Science Departament <br />
 * gr. 3241/1 <br />
 *
 * @author	Tudor Marian
 */
class Bmp2Gif
{
	
public:

	/**
	* Creates an object.
	*/
	Bmp2Gif();

	/**
	* Destroys the object.
	*/
	~Bmp2Gif();

	/**
	* Converts a gif image found into the <code>gifImage</code> serialized
	* form into a bitmap image.
	*
	* @ param	gifImage	the <code>ByteBuffer</code> object containing a
	*	gif image into the serialized form.
	* @return	the bitmap image obtained from the gif after the conversion
	*	porcess.
	*/
	BmpImage* convertGif2Bmp(ByteBuffer* gifImage);

	/**
	* Converts a gif file into a bitmap file, given a full memory
	* buffer with the gif image file.
	*
	* @param	gifImage	the serialized form of the gif image to be converted.
	* @return	the serialized form of the bitmap image obtained from the gif.
	*/
	ByteBuffer* serializeGif2Bmp(ByteBuffer* gifImage);

	/**
	* Converts a gif file into a bitmap file, given a GifImage
	* representaion of that object.
	*
	* @param	gif	the image object.
	* @return	the serialized form of the bitmap file obtained from the gif file.
	*/
	ByteBuffer* serializeGif2Bmp(GifImage* gif);

	/**
	* Converts a bitmap file into a gif file, given a full memory
	* buffer with the bitmap image file.
	*
	* @param	bmpImage	the serialized form of the bitmao image supposed to be
	*	converted into a gif file.
	* @return	the serialized form of the gif image obtained after the conversion
	*	process.
	*/
	ByteBuffer* serializeBmp2Gif(ByteBuffer* bmpImage);

	/**
	* Converts a bitmap file into a gif file, given a GifImage
	* representaion of that object.
	*
	* @param	bmp	the object representing the bitmap image tobe converted into
	*	a gif corespondent one.
	* @return	the serialized form of the gif image obtained after the conversion
	*	process.
	*/
	ByteBuffer* serializeBmp2Gif(BmpImage* bmp);

	/**
	* The method neded to calculate the real size of a bitmap scan line.
	*
	* @param	numBits	the number of bits the width of the image has.
	* @return	the size of the scan line.
	*/
	int rowBytes(int numBits);
};

#endif //_BMP2GIF_H_
