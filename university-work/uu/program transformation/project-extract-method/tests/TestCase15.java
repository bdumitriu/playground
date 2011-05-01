/**
 * Another test case with simple exception handling.
 */

import java.io.*;

public class TestCase15
{
	public static void main(String args[])
	{
		try
		{
			new TestCase15().testMethod(args[0]);
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
		while ((c = fis.read()) != -1)
		{
			System.out.write(c);
		}
		@
		fis.close();
	}
}
