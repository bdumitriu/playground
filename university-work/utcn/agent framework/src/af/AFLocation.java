package af;

/**
 * The location of an <code>AFAgent</code>
 *
 * Date: 26.05.2003
 * Time: 10:08:02
 * @author Tudor Marian,
 * @author email tudorm@c7.campus.utcluj.ro
 * @version 0.1
 */

public interface AFLocation extends java.io.Serializable
{
	public boolean equals(Object obj);

	public int hashCode();
}
