package client.dispatchers;

import client.interceptors.FriendInterceptor;
import client.contexts.FriendContext;
import client.contexts.FriendListContext;

import java.util.TreeMap;
import java.util.Map;
import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 20, 2005
 */
public class FriendDispatcher implements FriendInterceptor
{
	public static FriendDispatcher getInstance()
	{
		return instance;
	}

	public int addFriendInterceptor(FriendInterceptor interceptor)
	{
		interceptors.put(interceptorCount, interceptor);
		interceptorCount++;
		return interceptorCount;
	}

	public FriendInterceptor removeFriendInterceptor(int key)
	{
		return interceptors.remove(key);
	}

	public void friendAdded(FriendContext ctx)
	{
		invokeAll("friendAdded", ctx);
	}

	public void friendRemoved(FriendContext ctx)
	{
		invokeAll("friendRemoved", ctx);
	}

	public void wentOnline(FriendContext ctx)
	{
		invokeAll("wentOnline", ctx);
	}

	public void wentOffline(FriendContext ctx)
	{
		invokeAll("wentOffline", ctx);
	}

	public void receivedFriendsList(FriendListContext ctx)
	{
		for (FriendInterceptor interceptor : interceptors.values())
		{
			interceptor.receivedFriendsList(ctx);
		}
	}

	private void invokeAll(String methodName, FriendContext ctx)
	{
		try
		{
			Method method = FriendInterceptor.class.getMethod(methodName, FriendContext.class);
			for (FriendInterceptor interceptor : interceptors.values())
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

	private FriendDispatcher()
	{
		interceptorCount = 0;
		interceptors = new TreeMap<Integer, FriendInterceptor>();
	}

	protected Map<Integer, FriendInterceptor> interceptors;
	protected int interceptorCount;

	private static FriendDispatcher instance = new FriendDispatcher();
}
