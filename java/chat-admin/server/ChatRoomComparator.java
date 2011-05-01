package chatAdmin.server;

import java.util.Comparator;
import java.io.Serializable;
import chatAdmin.ChatRoom;

public class ChatRoomComparator implements Comparator, Serializable
{
	public int compare (Object o1, Object o2)
	{
		if (!((o1 instanceof ChatRoom) && (o2 instanceof ChatRoom)))
			throw new ClassCastException("At least one of the " +
			"arguments was not of the ChatRoom type");

		ChatRoom c1 = (ChatRoom) o1;
		ChatRoom c2 = (ChatRoom) o2;
		byte p1 = c1.getPriority();
		byte p2 = c2.getPriority();

		if (p1 < p2)
			return -1;
		else
			if (p1 == p2)
				return 0;
			else
				return 1;
	}
}
