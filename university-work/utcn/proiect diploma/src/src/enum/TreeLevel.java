package enum;

import java.io.Serializable;

/**
 * Definitions for tree levels.
 */
public class TreeLevel implements Serializable
{
	public static final int document = 0;
	public static final int paragraph = 1;
	public static final int sentence = 2;
	public static final int word = 3;
}
