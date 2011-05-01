import java.io.InputStream;
import java.io.OutputStream;
import java.io.IOException;

public class Echo
{
	public static void main(String a[])
	{
		InputStream in = System.in;
		OutputStream out = System.out;
		int ch;
		
		try
		{
			while ((ch = in.read()) != -1)
				out.write(ch);	
		}
		catch (IOException e)
		{
			System.err.println("Eroare la operatii I/O.");
			System.exit(1);
		}
	}
}
