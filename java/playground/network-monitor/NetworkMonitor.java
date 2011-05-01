import java.net.ServerSocket;
import java.net.Socket;
import java.io.*;

/**
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Jun 2, 2005
 */
public class NetworkMonitor
{
	public static void main(String[] args) throws IOException
	{
		new NetworkMonitor(80, 5000, "www.gogole.com").run();
	}

	public NetworkMonitor(int outPort, int inPort, String remoteHost)
	{
		this.outPort = outPort;
		this.inPort = inPort;
		this.remoteHost = remoteHost;
	}

	public void run() throws IOException
	{
		ServerSocket ss = new ServerSocket(inPort);
		while (true)
		{
			Socket ls = ss.accept();
			Socket rs = new Socket(remoteHost, outPort);

			new Thread(new PipelineThread(ls, rs, "log-out")).start();
			new Thread(new PipelineThread(rs, ls, "")).start();
		}
	}

	class PipelineThread implements Runnable
	{
		public PipelineThread(Socket inSocket, Socket outSocket, String logFile)
		{
			this.inSocket = inSocket;
			this.outSocket = outSocket;
			if (logFile != null && !logFile.equals(""))
			{
				this.logFile = logFile;
			}
			else
			{
				this.logFile = null;
			}
		}

		public void run()
		{
			try
			{
				InputStream is = inSocket.getInputStream();
				OutputStream os = outSocket.getOutputStream();
				OutputStream logOs = null;
				if (logFile != null)
				{
					logOs = new BufferedOutputStream(new FileOutputStream(logFile), 100000);
				}
				while (!inSocket.isClosed() && !outSocket.isClosed())
				{
					int c = is.read();
					os.write(c);
					if (logFile != null)
					{
						logOs.write(c);
						logOs.flush();
					}
				}
				logOs.close();
				if (inSocket.isClosed())
				{
					outSocket.close();
				}
			}
			catch (IOException e)
			{}
		}

		private Socket inSocket;
		private Socket outSocket;
		private String logFile;
	}

	private int outPort;
	private int inPort;
	private String remoteHost;
}
