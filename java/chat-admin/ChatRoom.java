package chatAdmin;

import java.io.Serializable;
import java.util.Vector;

/**
 * This class is used to store chat room related information.
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@email.ro
 * @version 1.0
 */
public class ChatRoom implements Serializable, Cloneable
{
	/*
	 * the name of the chat room.
	 */
	private String name;

	/*
	 * a short description of the chat room's profile.
	 */
	private String description;

	/*
	 * the login name of the user who ownns this chat room.
	 */
	private String ownerLoginName;

	/*
	 * the maximum number of people that can connect to the chat room.
	 */
	private byte maxUsersAllowed;

	/*
	 * the policy message that the chat server will send to any user who
	 * connects to this chat room.
	 */
	private String policyMessage;

	/*
	 * the Log which the owner of the chat room keeps.
	 */
	private Log userLog;

	/*
	 * the Log which the "admin" (superuser) keeps.
	 */
	private Log adminLog;

	/*
	 * the priority of this chat room in the list of priorities of the
	 * user that owns it. This priority is used when the "admin" makes
	 * the maxChatRooms of an user smaller and one or several chat rooms
	 * that belong to that user have to be deleted as a consequence.
	 */
	private byte priority;
	
	/*
	 * an id used in order to be able to uniquely identify chat rooms.
	 */
	private int id;

	/**
	 * Builds a new ChatRoom with the specified name and owner. Its
	 * description and policyMessage will be set to "", its maxUsersAllowed
	 * to 0 (unlimited), its priority to 127 and it will not be logged by
	 * either the owner or the "admin".
	 */
	public ChatRoom(String name, String ownerLoginName)
	{
		this.name = name;
		this.ownerLoginName = ownerLoginName;
		description = "";
		maxUsersAllowed = 0;
		policyMessage = "";
		userLog = null;
		adminLog = null;
		priority = 127;
		id = 0;
	}

	/**
	 * Sets the name of this chat room to name.
	 */
	public void setName(String name)
	{
		this.name = name;
	}

	/**
	 * Returns the name of this chat room.
	 */
	public String getName()
	{
		return name;
	}

	/**
	 * Sets the description of this chat room to description.
	 */
	public void setDescription(String description)
	{
		this.description = description;
	}

	/**
	 * Returns the description of this chat room.
	 */
	public String getDescription()
	{
		return  description;
	}

	/**
	 * Sets the login name of the owner of this chat room to ownerLoginName.
	 */
	public void setOwnerLoginName(String ownerLoginName)
	{
		this.ownerLoginName = ownerLoginName;
	}

	/**
	 * Returns the login name of the owner of this chat room.
	 */
	public String getOwnerLoginName()
	{
		return ownerLoginName;
	}

	/**
	 * Sets the maximum number of users allowed to connect to this chat
	 * room at a time to maxUsersAllowed.
	 */
	public void setMaxUsersAllowed(byte maxUsersAllowed)
	{
		this.maxUsersAllowed = maxUsersAllowed;
	}

	/**
	 * Returns the maximum number of users allowed to connect to this chat
	 * room at a time.
	 */
	public byte getMaxUsersAllowed()
	{
		return maxUsersAllowed;
	}

	/**
	 * Sets the policy message of this chat room to policyMessage.
	 */
	public void setPolicyMessage(String policyMessage)
	{
		this.policyMessage = policyMessage;
	}

	/**
	 * Returns the policy message of this chat room.
	 */
	public String getPolicyMessage()
	{
		return policyMessage;
	}

	/**
	 * Returns true if this chat room is logged by its owner. This means
	 * that if the userLog property is anything but null, this method will
	 * return true. That doesn't guarantee, however, that the file name
	 * stored in the {@link Log} object returned by the {@link
	 * #getUserLog()} method is a valid file name. As stated before, what
	 * this method returns is equivalent to:
	 * <pre>
	 * 	(this.getUserLog() != null)
	 * </pre>
	 * without any other checks on the contents of the userLog object.
	 */
	public boolean getUserLogged()
	{
		return (getUserLog() != null);
	}

	/**
	 * Returns true if this chat room is logged by "admin". This means
	 * that if the adminLog property is anything but null, this method will
	 * return true. That doesn't guarantee, however, that the file name
	 * stored in the {@link Log} object returned by the {@link
	 * #getAdminLog()} method is a valid file name. As stated before, what
	 * this method returns is equivalent to:
	 * <pre>
	 * 	(this.getAdminLog() != null)
	 * </pre>
	 * without any other checks on the contents of the adminLog object.
	 */
	public boolean getAdminLogged()
	{
		return (getAdminLog() != null);
	}

	/**
	 * Sets the Log object to which the owner of this chat room logs its
	 * activity to userLog.
	 */
	public void setUserLog(Log userLog)
	{
		this.userLog = userLog;
	}

	/**
	 * Returns the Log object to which the owner of this chat room logs its
	 * activity.
	 */
	public Log getUserLog()
	{
		if (userLog != null)
			return (Log) userLog.clone();
		else
			return null;
	}

	/**
	 * Sets the Log object to which the "admin" (superuser) logs this chat
	 * room's activity to adminLog.
	 */
	public void setAdminLog(Log adminLog)
	{
		this.adminLog = adminLog;
	}

	/**
	 * Returns the Log object to which the "admin" (superuser) logs this
	 * chat room's activity.
	 */
	public Log getAdminLog()
	{
		if (adminLog != null)
			return (Log) adminLog.clone();
		else
			return null;
	}

	/**
	 * Sets the priority of this chat room to priority. This priority will
	 * be used when the "admin" makes the maxChatRooms of an user smaller
	 * and one or several chat rooms that belong to that user have to be
	 * deleted as a consequence.
	 */
	public void setPriority(byte priority)
	{
		this.priority = priority;
	}

	/**
	 * Returns the priority of this chat room. This priority will be used
	 * when the "admin" makes the maxChatRooms of an user smaller and one
	 * or several chat rooms that belong to that user have to be deleted
	 * as a consequence.
	 */
	public byte getPriority()
	{
		return priority;
	}
	
	/**
	 * Sets the id of the chat room to id.
	 */
	public void setId(int id)
	{
		this.id = id;
	}

	/**
	 * Returns the id of the chat room.
	 */
	public int getId()
	{
		return id;
	}
	
	/**
	 * Returns a copy of this object.
	 * <br><br>
	 */
	public Object clone()
	{
		ChatRoom clonedObject = new ChatRoom(this.name,
			this.ownerLoginName);
		clonedObject.setAdminLog(this.adminLog);
		clonedObject.setDescription(this.description);
		clonedObject.setMaxUsersAllowed(this.maxUsersAllowed);
		clonedObject.setPolicyMessage(this.policyMessage);
		clonedObject.setPriority(this.priority);
		clonedObject.setUserLog(this.userLog);
		return clonedObject;
	}
	
	/**
	 * The equals method returns true if the names of the two chat rooms
	 * are identical and false otherwise.
	 */
	public boolean equals(Object obj)
	{
		if (obj == null)
			return false;
		if (!(obj instanceof ChatRoom))
			return false;
		ChatRoom chatRoom = (ChatRoom) obj;
		
		return (name.equals(chatRoom.name));
	}
	
	/**
	 * Returns the name of the chat room, its owner and its priority,
	 * separated by \t characters.
	 */
	public String toString()
	{
		return name;
	}
}
