package data;

import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import java.awt.*;

/**
 * Container class for ip - user - color mapping.
 * <br /><br />
 * Date: Mar 8, 2004
 * 
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class EditorUser
{
	public EditorUser()
	{
		this.ip = "";
		this.username = "";
		this.color = null;
		this.attributeSet = null;
	}

	public EditorUser(EditorUser editoruser)
	{
		this.ip = editoruser.ip;
		this.username = editoruser.username;
		this.color = editoruser.color;
		this.attributeSet = editoruser.attributeSet;
	}

	public EditorUser(String ip, String username, Color color)
	{
		this.ip = ip;
		this.username = username;
		this.color = color;

		attributeSet = new SimpleAttributeSet();
		StyleConstants.setFontSize(attributeSet, 12);
		StyleConstants.setForeground(attributeSet, color);
	}

	public String getIp()
	{
		return ip;
	}

	public void setIp(String ip)
	{
		this.ip = ip;
	}

	public String getUsername()
	{
		return username;
	}

	public void setUsername(String username)
	{
		this.username = username;
	}

	public Color getColor()
	{
		return color;
	}

	public void setColor(Color color)
	{
		this.color = color;
		StyleConstants.setForeground(attributeSet, color);
	}

	public SimpleAttributeSet getAttributeSet()
	{
		return attributeSet;
	}

	public boolean equals(Object editorUser)
	{
		if (!(editorUser instanceof EditorUser))
		{
			return false;
		}

		EditorUser other = (EditorUser) editorUser;

		if (other.getIp().equals(getIp()) && (other.getUsername().equals(getUsername())) &&
			other.getColor().equals(getColor()))
		{
			return true;
		}
		else
		{
			return false;
		}
	}

	private String ip;
	private String username;
	private Color color;

	private SimpleAttributeSet attributeSet;
}
