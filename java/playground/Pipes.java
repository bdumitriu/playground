import java.io.*;

public class Pipes
{
	public static void main(String args[])
	{
		try
		{
			PipedOutputStream outGen = new PipedOutputStream();
			PipedInputStream inConv = new PipedInputStream(outGen);
			PipedOutputStream outConv = new PipedOutputStream();
			PipedInputStream main = new PipedInputStream(outConv);
			Generator gen = new Generator(outGen);
			Convertor conv = new Convertor(inConv, outConv);
			int ch;
			
			gen.setPriority(Thread.MIN_PRIORITY);
			//conv.setPriority(Thread.MAX_PRIORITY);
			gen.start();
			conv.start();
			
			while ((ch = main.read()) != -1)
				System.out.print((char) ch);
		}
		catch (IOException e) {}
	}
}

class Generator extends Thread
{
	private PipedOutputStream out;
	
	public Generator(PipedOutputStream out)
	{
		this.out = out;
	}
	
	public void run()
	{
		int ch;
		for (int i = 0; i < 1000; i++)
		{
			ch = (int) Math.round(Math.random()*100);
			try
			{
				sleep(10);
				if ((ch > 64) && (ch < 91))
					out.write(ch);
				if (ch == 32)
					out.write(ch);
			}
			catch (IOException e) {}
			catch (InterruptedException e) {}
		}
	}
}

class Convertor extends Thread
{
	private PipedInputStream in;
	private PipedOutputStream out;
	
	public Convertor(PipedInputStream in, PipedOutputStream out)
	{
		this.in = in;
		this.out = out;
	}

	public void run()
	{
		int ch, nr;
		
		try
		{
			while ((ch = in.read()) != -1)
			{
				nr = (int) Math.round(Math.random()*5);
				if ((nr == 0) || (ch == 32))
					out.write(ch);
				else
					out.write(ch+32);
			}
		}
		catch (IOException e) {}
	}
}
