package client.dispatchers;

import client.interceptors.MessageInterceptor;
import client.contexts.MessageContext;

import java.util.*;
import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 20, 2005
 */
public class MessageDispatcher implements MessageInterceptor
{
	public static MessageDispatcher getInstance()
	{
		return instance;
	}

	/**
	 * @param priority interceptors are called in increasing order of their priority for messageSent &
	 * messageReceived calls, and in decreasing order of their priority for preMessageSend.
	 *
	 */
	public void addMessageInterceptor(MessageInterceptor interceptor, int priority)
	{
		interceptors.put(priority, interceptor);
	}

	public MessageInterceptor removeMessageInterceptor(int priority)
	{
		return interceptors.remove(priority);
	}

	public void preMessageSend(MessageContext ctx)
	{
		ArrayList<MessageInterceptor> values = new ArrayList<MessageInterceptor>(interceptors.values());
		for (int i = values.size() - 1; i >= 0; i--)
		{
			values.get(i).preMessageSend(ctx);
		}
	}

	public void messageSent(MessageContext ctx)
	{
		invokeAll("messageSent", ctx);
	}

	public void messageReceived(MessageContext ctx)
	{
		invokeAll("messageReceived", ctx);
	}

	private void invokeAll(String methodName, MessageContext ctx)
	{
		try
		{
			Method method = MessageInterceptor.class.getMethod(methodName, MessageContext.class);
			for (MessageInterceptor interceptor : interceptors.values())
			{
				method.invoke(interceptor, ctx);
			}
		}
		catch (NoSuchMethodException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
		catch (IllegalAccessException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
		catch (InvocationTargetException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
	}

	private MessageDispatcher()
	{
		interceptors = new TreeMap<Integer, MessageInterceptor>();
	}

	protected Map<Integer, MessageInterceptor> interceptors;

	private static MessageDispatcher instance = new MessageDispatcher();
}

