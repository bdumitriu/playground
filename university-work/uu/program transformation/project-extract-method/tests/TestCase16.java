/**
 * A test case with more complex exception handling.
 */

import java.io.*;
import java.net.*;

public class TestCase16
{
	public static void main(String args[])
	{
		try
		{
			new TestCase16().testMethod(args[0]);
		}
		catch (Exception e)
		{}
	}

	public void testMethod(String name) throws IOException, Exception
	{
		File f = new File(name);
		FileInputStream fis = new FileInputStream(f);
		int c;
		@ emTestMethod
		f.clone();
		try
		{
			f.toURL();
			while ((c = fis.read()) != -1)
			{
				System.out.write(c);
			}

			try
			{
				new URI("bla").parseServerAuthority();
				fis.read();
			}
			catch (IOException e)
			{}
		}
		catch (MalformedURLException e)
		{}
		catch (URISyntaxException e)
		{}
		@
		fis.close();
	}
}
