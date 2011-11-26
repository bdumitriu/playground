package client.interceptors;

import client.contexts.LoginContext;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 19, 2005
 */
public interface LoginInterceptor
{
	public void preLogin(LoginContext ctx);

	public void postLogin(LoginContext ctx);

	public void preLogout(LoginContext ctx);

	public void postLogout(LoginContext ctx);
}
