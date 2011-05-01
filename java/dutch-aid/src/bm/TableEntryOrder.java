package bm;

/**
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Jan 15, 2005
 *
 * Extend this interface in order to provide an index/attribute mapping to be used by a TableEntry.
 *
 * Be aware that the instance of this interface is only used in the constructor, so after creating a TableEntry, any
 * changes in the behaviour of {@link #elementAt} will not influence the table entry anymore.
 */
public interface TableEntryOrder
{
	/**
	 * Given an index value, return the attribute to be placed on that specific position in a Vector. This is used
	 * for creating the order of display of attributes in a table. Use the constants defined in the interface as
	 * return values.
	 *
	 * If you want, for example, the <code>article</code> attribute to be returned by TableEntry's elementAt(2)
	 * then implement your class' elementAt such that when it is called with the value 2, it returns
	 * TableOrderEntry.article. Numbering is 0-based.
	 */
	public int elementAt(int index);

	/**
	 * This is the opposite of the {@link #elementAt} method, i.e., you get one of <code>dutchWord</code>,
	 * <code>englishWord</code>, etc. as a parameter and you have to return the index the attribute is mapped to.
	 * Make sure that the behaviours of {@link #elementAt} and this method are consistent, i.e.
	 * n = indexFor(elementAt(n)) and the other way around.
	 */
	public int indexFor(int attribute);

	public static final int dutchWord = 0;
	public static final int englishWord = 1;
	public static final int romanianWord = 2;
	public static final int type = 3;
	public static final int article = 4;
	public static final int frequency = 5;
	public static final int extra = 6;
	public static final int sampleExpression = 7;
}
