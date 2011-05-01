package jade;

import af.AFAgentID;

/**
 * Created by IntelliJ IDEA.
 *
 * Date: May 19, 2003
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class JadeAgentID extends AFAgentID
{
	public JadeAgentID(String name)
	{
		super();
		addProperty("name", name);
	}

	public String getID()
	{
		return getProperty("name");
	}
}