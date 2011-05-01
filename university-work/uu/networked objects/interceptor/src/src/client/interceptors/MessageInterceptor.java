package client.interceptors;

import client.contexts.MessageContext;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 20, 2005
 */
public interface MessageInterceptor
{
	public void preMessageSend(MessageContext ctx);

	public void messageSent(MessageContext ctx);

	public void messageReceived(MessageContext ctx);
}
