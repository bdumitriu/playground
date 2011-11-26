package client.interceptors;

import client.contexts.FriendContext;
import client.contexts.FriendListContext;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 20, 2005
 */
public interface FriendInterceptor
{
	public void friendAdded(FriendContext ctx);

	public void friendRemoved(FriendContext ctx);

	public void wentOnline(FriendContext ctx);

	public void wentOffline(FriendContext ctx);

	public void receivedFriendsList(FriendListContext ctx);
}
