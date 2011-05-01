package bm;

/**
 * Data container class for a complete entry in the dictionary.
 * <br /><br />
 * Date: Aug 12, 2004
 * 
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class Entry implements Comparable
{
	/**
	 * Creates a new entry with empty strings for each value. Sort ordering is intialized to
	 * <code>dutchOrdering</code>.
	 */
	public Entry()
	{
		this("", "", "", "", "", "", "", "");
	}

	/**
	 * Creates a new entry with the specified values. Sort ordering is intialized to <code>dutchOrdering</code>.
	 *
	 * @param dutchWord
	 * @param romanianWord
	 * @param englishWord
	 * @param type
	 * @param article
	 * @param frequency
	 * @param extra
	 * @param sampleExpression
	 */
	public Entry(String dutchWord, String romanianWord, String englishWord, String type, String article, String frequency, String extra, String sampleExpression)
	{
		this.dutchWord = dutchWord;
		this.romanianWord = romanianWord;
		this.englishWord = englishWord;
		this.type = type;
		this.article = article;
		this.frequency = frequency;
		this.extra = extra;
		this.sampleExpression = sampleExpression;
		this.sortOrder = dutchOrdering;
	}

	/**
	 * Copy constructor.
	 */
	public Entry(Entry entry)
	{
		this(entry.dutchWord, entry.romanianWord, entry.englishWord, entry.type, entry.article, entry.frequency, entry.extra, entry.sampleExpression);
	}

	public String getDutchWord()
	{
		return dutchWord;
	}

	public void setDutchWord(String dutchWord)
	{
		this.dutchWord = dutchWord;
	}

	public String getRomanianWord()
	{
		return romanianWord;
	}

	public void setRomanianWord(String romanianWord)
	{
		this.romanianWord = romanianWord;
	}

	public String getEnglishWord()
	{
		return englishWord;
	}

	public void setEnglishWord(String englishWord)
	{
		this.englishWord = englishWord;
	}

	public String getType()
	{
		return type;
	}

	public void setType(String type)
	{
		this.type = type;
	}

	public String getArticle()
	{
		return article;
	}

	public void setArticle(String article)
	{
		this.article = article;
	}

	public String getFrequency()
	{
		return frequency;
	}

	public void setFrequency(String frequency)
	{
		this.frequency = frequency;
	}

	public String getExtra()
	{
		return extra;
	}

	public void setExtra(String extra)
	{
		this.extra = extra;
	}

	public String getSampleExpression()
	{
		return sampleExpression;
	}

	public void setSampleExpression(String sampleExpression)
	{
		this.sampleExpression = sampleExpression;
	}

	public int getSortOrder()
	{
		return sortOrder;
	}

	/**
	 * The sort order parameter indicates what attribute has to be compared in the {@link #compareTo} method.
	 * <code>dutchOrdering</code> implies that the <code>dutch</code> attributes will be compared and so on.
	 * This customization is available in order to allow you to choose what kind of sort you want to do. Make sure,
	 * however, that you set the same value of the <code>sortOrder</code> attribute for all objects in a collection
	 * which is to be sorted, or else you will most probably end up with a non-sorted output.
	 *
	 * @param sortOrder an indication of what attribute you want compared in the {@link #compareTo} method
	 */
	public void setSortOrder(int sortOrder)
	{
		this.sortOrder = sortOrder;
	}

	public int compareTo(Object o)
	{
		if (!(o instanceof Entry))
		{
			throw new ClassCastException();
		}

		Entry entry = (Entry) o;

		switch(sortOrder)
		{
			case englishOrdering:
			{
				return englishWord.compareTo(entry.englishWord);
			}
			case romanianOrdering:
			{
				return romanianWord.compareTo(entry.romanianWord);
			}
			default:
			{
				return dutchWord.compareTo(entry.dutchWord);
			}
		}
	}

	protected String dutchWord;
	protected String romanianWord;
	protected String englishWord;
	protected String type;
	protected String article;
	protected String frequency;
	protected String extra;
	protected String sampleExpression;

	protected int sortOrder;

	public static final int dutchOrdering = 0;
	public static final int englishOrdering = 1;
	public static final int romanianOrdering = 2;
}
