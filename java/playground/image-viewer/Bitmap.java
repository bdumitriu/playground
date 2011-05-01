/*
 * Author: Bogdan DUMITRIU
 * Date:   28.02.2002
 */

import java.io.*;
import javax.swing.*;
import java.awt.*;
import java.awt.image.*;

class Bitmap extends JPanel
{
	public Bitmap(String filename)
	{
		byte b0[] = new byte[1];
		byte b1[] = new byte[2];
		byte b2[] = new byte[4];
		byte b3[] = new byte[3];
		byte b[];
		
		this.filename = filename;

		try
		{
			//BufferedInputStream is = new BufferedInputStream(new FileInputStream(filename));
			FileInputStream fis = new FileInputStream(filename);
			int av = fis.available();
			b = new byte[av];
			fis.read(b);
			fis.close();
			ByteArrayInputStream is = new ByteArrayInputStream(b);
			
			// read various stuff from file...
			is.skip(2);
			is.read(b2);
			size = byteToInt(b2[3])*16777216+byteToInt(b2[2])*65536+byteToInt(b2[1])*256+byteToInt(b2[0]);
			is.skip(4);
			is.read(b2);
			dataOffset = byteToInt(b2[3])*16777216+byteToInt(b2[2])*65536+byteToInt(b2[1])*256+byteToInt(b2[0]);
			is.skip(4);
			is.read(b2);
			width = byteToInt(b2[3])*16777216+byteToInt(b2[2])*65536+byteToInt(b2[1])*256+byteToInt(b2[0]);
			is.read(b2);
			height = byteToInt(b2[3])*16777216+byteToInt(b2[2])*65536+byteToInt(b2[1])*256+byteToInt(b2[0]);
			is.skip(2);
			is.read(b1);
			cDepth = byteToInt(b1[1])*256+byteToInt(b1[0]);
			is.read(b2);
			compression = byteToInt(b2[3])*16777216+byteToInt(b2[2])*65536+byteToInt(b2[1])*256+byteToInt(b2[0]);
			is.skip(12);
			is.read(b2);
			nrColors = byteToInt(b2[3])*16777216+byteToInt(b2[2])*65536+byteToInt(b2[1])*256+byteToInt(b2[0]);
			is.skip(4);
			
			// read table of colors
			int n = nrColors;
			if ((n == 0) && (cDepth <= 8))
				n = (int) Math.pow(2, cDepth);

			colors = new Color[n];
			for (int i = 0; i < n; i++)
			{
				is.read(b2);
				colors[i] = new Color(byteToInt(b2[2]), byteToInt(b2[1]), byteToInt(b2[0]));
			}

			// see how many "padding" bytes we have at the end of each line
			int skipVal = width % 4;
			if (skipVal != 0)
				skipVal = 4 - skipVal;

			if (cDepth <= 8)
			{
				image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
				for (int i = height-1; i >= 0; i--)
				{
					for (int j = 0; j < width; j++)
					{
						is.read(b0);
						image.setRGB(j, i, colors[byteToInt(b0[0])].getRGB());
					}

					// skip the "padding" bytes
					is.skip(skipVal);
				}
			}
			else
			{
				image = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR);
				for (int i = height-1; i >= 0; i--)
				{
					for (int j = 0; j < width; j++)
					{
						is.read(b3);
						image.setRGB(j, i, byteToInt(b3[2])*65536 + byteToInt(b3[1])*256 +
							byteToInt(b3[0]));
					}
				}
			}
				
			is.close();
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		
		setSize(width, height);
	}
	
	public int getWidth()
	{
		return width;
	}
	
	public int getHeight()
	{
		return height;
	}
	
	public int getFileSize()
	{
		return size;
	}
	
	public int getColorDepth()
	{
		return cDepth;
	}
	
	public int getCompression()
	{
		return compression;
	}
	
	public int getNumberOfColorsUsed()
	{
		return nrColors;
	}
	
	public void paintComponent(Graphics g)
	{
		super.paintComponent(g);
		
		((Graphics2D) g).drawImage(image, null, 0, 0);
	}

	public static void main(String args[])
	{
		if (args.length != 1)
		{
			System.out.println("Usage: java Bitmap image.bmp");
			System.exit(0);
		}
		String filename = args[0];
		long startTime = System.currentTimeMillis();
		Bitmap bmpImg = new Bitmap(filename);
		JFrame f = new JFrame(filename);
		f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		f.setSize(bmpImg.getWidth(), bmpImg.getHeight());
		f.getContentPane().add(bmpImg);
		f.setVisible(true);
		long stopTime = System.currentTimeMillis();
		System.out.println("Load time: " + (stopTime - startTime));
	}

	private int byteToInt(byte b)
	{
		if (b >= 0)
			return b;
		else
			return b+256;
	}

	private String filename;
	private int width;
	private int height;
	private int size;
	private int cDepth;
	private int compression;
	private int nrColors;
	private int dataOffset;
	private Color colors[];
	private BufferedImage image;
}

