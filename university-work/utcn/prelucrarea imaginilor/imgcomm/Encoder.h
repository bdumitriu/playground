
#ifndef _ENCODER_H_
#define _ENCODER_H_

#include "Bmp2Gif.h"

/**
 * This is the top class, the upper layer relies upon it's methods
 * to do the compression. <br /><br />
 *
 *
 * Author Tudor Marian <br />
 *
 * Technical University Of Cluj-Napoca <br />
 * Computer Science Departament <br />
 * gr. 3241/1 <br />
 * created: mai, 12, 2003 <br />
 *
 * @Author Tudor Marian
 */

class Encoder
{
	public:
		Encoder();
		~Encoder();
		
		/**
		 * Encodes a 8 bit bitmap image or grayscale image into a gif
		 * equivalent image.
		 *
		 * @param	<code>bitmapImage</code>	holds the bitmap image
		 * @param	<code>bitmapSize</code>	holds the size of the bitmap.
		 * @param	<code>gifImage</code>	will contain upon return the
		 *		gif image.
		 * @param	<code>gifSize</code>	will contain upon return the gif
		 *		image's size.
		 * @return	the size of the encoded file.
		 */
		static int encodeGrayscaleImage(byte* bitmapImage, long bitmapSize, 
			byte** gifImage, long* gifSize);
		
		/**
		 * Decodes a grayscale or any 8 bit image found under the gif
		 * format into a 8 bit depth coresponding bitmap file.
		 *
		 * @param	<code>gifImage</code>	the binary representation of the
		 *		encoded gif image.
		 * @param	<code>gifSize</code>	the size of the gif image.
		 * @param	<code>bitmapImage</code>	 it will contain the returned
		 *		value of the decoded image.
		 * @param	<code>bitmapSize</code>	 holds the size of the decoded image.
		 * @return	the size of the decoded file.
		 */
		static int decodeGrayscaleImage(byte* gifImage, long gifSize, 
			byte** bitmapImage, long* bitmapSize);
		
		/**
		 * Encodes a full color image compressed with the LZW algorithm.
		 *
		 * @param	<code>bitmapImage</code>	 holds the bitmap image
		 * @param	<code>bitmapSize</code>		holds the size of the bitmap.
		 * @param	<code>compressedImage</code>	will contain upon return the
		 *		compressed image.
		 * @param	<code>compressedImageSize</code> will contain upon return
		 *		the compressed image's size.
		 * @return	the size of the encoded file.
		 */
		static int encodeHighColorImage(byte* bitmapImage, long bitmapSize, 
			byte** compressedImage, long* compressedImageSize);
		
		/**
		 * Decodes a full color image compressed with the LZW algorithm.
		 *
		 * @param	<code>compressedImage</code>	the binary representation of the
		 *		encoded image.
		 * @param	<code>compressedImageSize</code>	 the size of the compressed
		 *		image.
		 * @param	<code>bitmapImage</code>	 it will contain the returned
		 *		value of the decoded image.
		 * @param	<code>bitmapSize</code>	 holds the size of the decoded image.
		 * @return	the size of the decoded file.
		 */
		static int decodeHighColorImage(byte* compressedImage, long compressedImageSize, 
			byte** bitmapImage, long* bitmapSize);
};

#endif // _ENCODER_H_

