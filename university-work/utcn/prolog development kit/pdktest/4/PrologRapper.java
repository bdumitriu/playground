
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 *
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@bdumitriu.ro
 * @version 0.1
 * 
 * Date: Mar 10, 2003
 */
public class PrologRapper
{
	private static String sPath = "c:\\Utils\\Programming\\SICStus Prolog\\bin\\sicstus.exe -i";

	private InputStream stdin;

	private InputStream stderr;

	private OutputStream stdout;

	private Process p;

	public static void main(String[] args)
	{
		new PrologRapper().run();
	}

	public PrologRapper()
	{
		try
		{
			p = Runtime.getRuntime().exec(sPath);
			stdin = p.getInputStream();
			stderr = p.getErrorStream();
			stdout = p.getOutputStream();
		}
		catch (IOException e)
		{
			System.out.println("IOException occured!");
		}
	}

	private void run()
	{
		PrologReader rdr = new PrologReader(stdin, stderr);
		
		rdr.receive();

		byte[] buf = new byte[1024];
		while (true)
		{
			try
			{
				int count = System.in.read(buf);
				stdout.write(buf, 0, count);
				stdout.flush();
				if (new String(buf, 0, count).toString().indexOf("halt.") != -1)
				{
					break;
				}
				else
				{
					rdr.receive();
				}
			}
			catch (IOException e)
			{
				System.out.println("IOException occured!");
			}
		}

		p.destroy();
	}

	class PrologReader
	{
		private InputStream _stdin, _stderr;


		public PrologReader(InputStream is1, InputStream is2)
		{
			_stdin = is1;
			_stderr = is2;
		}

		public String receive()
		{
			int b;
			try
			{
				while (_stderr.available() == 0)
				{
					if (_stdin.available() > 0)
					{
						b = _stdin.read();
						System.out.print((char) b);
					}
				}

				while (_stdin.available() != 0)
				{
					b = _stdin.read();
					System.out.print((char) b);
				}

				//Thread.sleep(1);
				while (_stderr.available() != 0)
				{
					b = _stderr.read();
					System.out.print((char) b);
				}
			}
			catch (IOException e)
			{
				System.out.println("IO Exception occured!");
			}

			return null;
		}
	}
}
