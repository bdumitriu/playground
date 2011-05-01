
/**
 *
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Jan 4, 2004
 */
class VoteRecord
{
	public VoteRecord(String title, String director, float voteAvg, int voteCount)
	{
		this.title = title;
		this.director = director;
		this.voteAvg = voteAvg;
		this.voteCount = voteCount;
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

	public float getVoteAvg()
	{
		return voteAvg;
	}

	public void setVoteAvg(float voteAvg)
	{
		this.voteAvg = voteAvg;
	}

	public int getVoteCount()
	{
		return voteCount;
	}

	public void setVoteCount(int voteCount)
	{
		this.voteCount = voteCount;
	}

	private String title;
	private String director;
	private float voteAvg;
	private int voteCount;
}

