/**
 *
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Jan 4, 2004
 */
public class MovieRecord
{
	public MovieRecord(int id, String title)
	{
		this.id = id;
		this.title = title;
	}

	public int getId()
	{
		return id;
	}

	public void setId(int id)
	{
		this.id = id;
	}

	public String getTitle()
	{
		return title;
	}

	public void setTitle(String title)
	{
		this.title = title;
	}

	private int id;
	private String title;
}