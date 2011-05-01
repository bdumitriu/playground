/**
 * A test case with simple exception handling.
 */

import java.io.*;
import java.net.MalformedURLException;

public class TestCase14
{
	public static void main(String args[])
	{
		try
		{
			new TestCase14().testMethod(args[0]);
		}
		catch (IOException e)
		{}
	}

	public void testMethod(String name) throws IOException
	{
		File f = new File(name);
		FileInputStream fis = new FileInputStream(f);
		int c;
		@ emTestMethod
		try
		{
			f.toURL();
			while ((c = fis.read()) != -1)
			{
				System.out.write(c);
			}
		}
		catch (MalformedURLException e)
		{}
		finally
		{}
		@
		fis.close();
	}
}
