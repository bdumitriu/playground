package client.dispatchers;

import client.interceptors.LoginInterceptor;
import client.contexts.LoginContext;

import java.util.Map;
import java.util.TreeMap;
import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 19, 2005
 */
public class LoginDispatcher implements LoginInterceptor
{
	public static LoginDispatcher getInstance()
	{
		return instance;
	}

	public int addLoginInterceptor(LoginInterceptor interceptor)
	{
		interceptors.put(interceptorCount, interceptor);
		interceptorCount++;
		return interceptorCount;
	}

	public LoginInterceptor removeLoginInterceptor(int key)
	{
		return interceptors.remove(key);
	}

	public void preLogin(LoginContext ctx)
	{
		invokeAll("preLogin", ctx);
	}

	public void postLogin(LoginContext ctx)
	{
		invokeAll("postLogin", ctx);
	}

	public void preLogout(LoginContext ctx)
	{
		invokeAll("preLogout", ctx);
	}

	public void postLogout(LoginContext ctx)
	{
		invokeAll("postLogout", ctx);
	}

	private void invokeAll(String methodName, LoginContext ctx)
	{
		try
		{
			Method method = LoginInterceptor.class.getMethod(methodName, LoginContext.class);
			for (LoginInterceptor interceptor : interceptors.values())
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

	private LoginDispatcher()
	{
		interceptorCount = 0;
		interceptors = new TreeMap<Integer, LoginInterceptor>();
	}

	protected Map<Integer, LoginInterceptor> interceptors;
	protected int interceptorCount;

	private static LoginDispatcher instance = new LoginDispatcher();
}
