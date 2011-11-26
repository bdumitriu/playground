package client.interceptors;

import client.contexts.MessageContext;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 20, 2005
 */
public class Translator implements MessageInterceptor
{
	public Translator(boolean dir)
	{
		this.dir = dir;
	}

	public boolean isDir()
	{
		return dir;
	}

	public void setDir(boolean dir)
	{
		this.dir = dir;
	}

	public void preMessageSend(MessageContext ctx)
	{
		String msg = ctx.getMessage();

		if (dir) // Dutch to English
		{
			msg = msg.replaceAll("ik", "I");
			msg = msg.replaceAll("huis", "house");
			msg = msg.replaceAll("bericht", "message");
			msg = msg.replaceAll("zon", "sun");
		}
		else
		{
			msg = msg.replaceAll("I ", "ik ");
			msg = msg.replaceAll("house", "huis");
			msg = msg.replaceAll("message", "bericht");
			msg = msg.replaceAll("sun", "zon");
		}

		ctx.setMessage(msg);
	}

	public void messageSent(MessageContext ctx)
	{}

	public void messageReceived(MessageContext ctx)
	{
		String msg = ctx.getMessage();

		if (!dir)
		{
			msg = msg.replaceAll("ik", "I");
			msg = msg.replaceAll("huis", "house");
			msg = msg.replaceAll("bericht", "message");
			msg = msg.replaceAll("zon", "sun");
		}
		else
		{
			msg = msg.replaceAll("I ", "ik ");
			msg = msg.replaceAll("house", "huis");
			msg = msg.replaceAll("message", "bericht");
			msg = msg.replaceAll("sun", "zon");
		}

		ctx.setMessage(msg);
	}

	private boolean dir;
}
