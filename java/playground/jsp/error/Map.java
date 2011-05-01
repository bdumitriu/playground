package email;

import java.util.TreeMap;

public class Map extends TreeMap {
	
	private String name, email, action;
	private int count = 0;

	public Map() {}

	public void setName(String formName)
	{
		if (formName != "") {
			name = formName;
		}
	}

	public String getName() {
		return name;
	}

	public void setEmail(String formEmail) {
		if (formEmail != "") {
			email = formEmail;
		}
	}

	public String getEmail() {
		email = get(name).toString();
		return email;
	}

	public void setAction(String pageAction) {
		action = pageAction;
	}

	public String getAction() {
		return action;
	}
}