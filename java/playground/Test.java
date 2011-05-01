import java.io.*;

public class Test
{
	public static void main(String a[]) throws Throwable, IOException
	{
		throw new IOException("just testing...");
		System.out.println("see if we get here or not...");
	}

	public Test()
	{
		System.out.println("created");
	}

	protected void finalize() throws Throwable
	{
		try
		{
			FileOutputStream out = new FileOutputStream("xxx.dat");
			out.close();
		}
		catch (IOException e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
	}
}
