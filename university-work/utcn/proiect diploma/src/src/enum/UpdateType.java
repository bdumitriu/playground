package enum;

/**
 * Definitions for types of update.
 */
public class UpdateType
{
	/** the user is not asked about any conflicts; the local operation always wins */
	public static final int noChoice = 0;

	/** the user is presented with pairs of operations to choose from */
	public static final int operationChoice = 1;

	/**
	 * the user is presented with two different effects of the group of operations (the local and the
	 * remote group) which affect the same semantic unit
	 */
	public static final int conflictChoice = 2;

	/** this is only used internally; do not use for outside calls */
	public static final int noMerge = 3;
}
