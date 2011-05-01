/**
 *
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Jan 4, 2004
 */
class GrossRecord
{
	public GrossRecord(String title, String director, int totalGross)
	{
		this.title = title;
		this.director = director;
		this.totalGross = totalGross;
	}

	public String getTitle()
	{
		return title;
	}

	public void setTitle(String title)
	{
		this.title = title;
	}

	public String getDirector()
	{
		return director;
	}

	public void setDirector(String director)
	{
		this.director = director;
	}

	public int getTotalGross()
	{
		return totalGross;
	}

	public void setTotalGross(int totalGross)
	{
		this.totalGross = totalGross;
	}

	private String title;
	private String director;
	private int totalGross;
}

