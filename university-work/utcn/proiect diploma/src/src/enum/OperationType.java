package enum;

import java.io.Serializable;

/**
 * Definitions for types of operations.
 */
public class OperationType implements Serializable
{
	public static final int insert = 0;
	public static final int delete = 1;
	public static final int nop = 2;
}