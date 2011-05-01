import java.io.*;
import java.net.*;

public class WebPageReader
{
	public static void main(String args[])
	{
		try
		{
			URL url = new URL("http", "localhost", args[0]);
			String line;

			BufferedReader br =
				new BufferedReader
				(new InputStreamReader(url.openStream()));

			while ((line = br.readLine()) != null)
				System.out.println(line);
		}
		catch (MalformedURLException e)
		{
			System.out.println(e.toString());
		}
		catch (IOException e)
		{
			System.out.println(e.toString());
		}
	}
}
