package chatAdmin;

import java.io.Serializable;

/**
 * This class is used to store user related information.
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@email.ro
 * @version 1.0
 */
public class User implements Serializable, Cloneable
{
	/*
	 * the real name of the user.
	 */
	private String name;

	/*
	 * the password used by the user to log in.
	 */
	private String password;

	/*
	 * the login name that the user will use to log in.
	 */
	private String loginName;

	/*
	 * various contact info about the user.
	 */
	private String contactInfo;

	/*
	 * The maximum number of chat rooms a user can own. If it is 0
	 * then the user can have as many chat rooms as he wants.
	 */
	private byte maxChatRooms;

	/**
	 * Builds a new user with the specified login name and password.
	 * By default, the maximum number of chat rooms this user can own
	 * will be set to 0 (unlimited).
	 */
	public User(String loginName, String password)
	{
		this.loginName = loginName;
		this.password = password;
		name = "";
		contactInfo = "";
		maxChatRooms = 0;
	}

	/**
	 * Sets the name of this user to userName. Beware that this is not
	 * the login name, it is just the "real" name of the user, i.e. it will
	 * not be used for authentification, it will only have an informative
	 * purpose.
	 * <br><br>
	 * @see #setLoginName(String loginName) setLoginName()
	 */
	public void setName(String userName)
	{
		name = userName;
	}

	/**
	 * Returns the name of this user. Beware that this is not the login
	 * name, it is just the "real" name of the user, i.e. it will not be
	 * used for authentification, it will only have an informative
	 * purpose.
	 * <br><br>
	 * @see #getLoginName() getLoginName()
	 */
	public String getName()
	{
		return name;
	}

	/**
	 * Sets the login name of the user to loginName.
	 */
	public void setLoginName(String loginName)
	{
		this.loginName = loginName;
	}

	/**
	 * Returns the login name of this user.
	 */
	public String getLoginName()
	{
		return loginName;
	}

	/**
	 * Sets the password for this user to password.
	 */
	public void setPassword(String password)
	{
		this.password = password;
	}

	/**
	 * Returns the password of this user.
	 */
	public String getPassword()
	{
		return password;
	}

	/**
	 * Sets the contact info for this user to contactInfo.
	 */
	public void setContactInfo(String contactInfo)
	{
		this.contactInfo = contactInfo;
	}

	/**
	 * Returns the contact info for this user.
	 */
	public String getContactInfo()
	{
		return contactInfo;
	}

	/**
	 * Sets the maximum number of chat rooms this user can own to max.
	 */
	public void setMaximumNumberOfChatRooms(byte max)
	{
		maxChatRooms = max;
	}

	/**
	 * Returns the maximum number of chat rooms this user can own.
	 */
	public byte getMaximumNumberOfChatRooms()
	{
		return maxChatRooms;
	}
	
	/**
	 * Returns a copy of this object.
	 * <br><br>
	 */
	public Object clone()
	{
		User clonedObject = new User(this.loginName, this.password);
		clonedObject.setName(this.name);
		clonedObject.setContactInfo(this.contactInfo);
		clonedObject.setMaximumNumberOfChatRooms(this.maxChatRooms);
		return clonedObject;
	}
	
	/**
	 * Returns true if the loginNames of the two Users are identical and
	 * false otherwise.
	 * <br><br>
	 */
	public boolean equals(Object obj)
	{
		if (obj == null)
			return false;
		if (!(obj instanceof User))
			return false;
		User user = (User) obj;
		
		return (loginName.equals(user.loginName));
	}

	/**
	 * Returns the loginName and the "real" name separated by a \t
	 * character.
	 * <br><br>
	 */
	public String toString()
	{
		return loginName;
	}
}

