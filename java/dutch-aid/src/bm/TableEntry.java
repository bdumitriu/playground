package bm;

import java.util.Vector;
import static bm.TableEntryOrder.*;

/**
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Jan 15, 2005
 *
 * Saves Entry as a Vector (internally) so that it is fit for use in a JTable.
 */
public class TableEntry extends Vector<String> implements Comparable
{
	public TableEntry(Entry entry, TableEntryOrder order)
	{
		super(nrAttributes);
		this.order = order;

		for (int i = 0 ; i < nrAttributes; i++)
		{
			add(i, getDataForIndex(i, entry));
		}
	}

	private String getDataForIndex(int index, Entry entry)
	{
		switch(order.elementAt(index))
		{
			case dutchWord:
			{
				return entry.getDutchWord();
			}
			case englishWord:
			{
				return entry.getEnglishWord();
			}
			case romanianWord:
			{
				return entry.getRomanianWord();
			}
			case type:
			{
				return entry.getType();
			}
			case frequency:
			{
				return entry.getFrequency();
			}
			case article:
			{
				return entry.getArticle();
			}
			case sampleExpression:
			{
				return entry.getSampleExpression();
			}
			case extra:
			{
				return entry.getExtra();
			}
			default:
			{
				return "";
			}
		}
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
		if (!(o instanceof TableEntry))
		{
			throw new ClassCastException();
		}

		TableEntry entry = (TableEntry) o;

		switch(sortOrder)
		{
			case englishOrdering:
			{
				int index = order.indexFor(englishWord);
				return get(index).compareTo(entry.get(index));
			}
			case romanianOrdering:
			{
				int index = order.indexFor(romanianWord);
				return get(index).compareTo(entry.get(index));
			}
			default:
			{
				int index = order.indexFor(dutchWord);
				return get(index).compareTo(entry.get(index));
			}
		}
	}

	private TableEntryOrder order;
	private static final int nrAttributes = 8;	// nr. of data attributes in the Entry class

	protected int sortOrder;

	public static final int dutchOrdering = 0;
	public static final int englishOrdering = 1;
	public static final int romanianOrdering = 2;
}
