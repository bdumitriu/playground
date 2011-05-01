package gw.storage.javasvn;

/**
 * This exception indicates that you tried to perform some action on a node that does
 * not exist.
 *
 * @author Bogdan Dumitriu
 */
public class NoSuchNodeException extends Exception
{
    public NoSuchNodeException()
    {
        super();
    }

    public NoSuchNodeException(String message)
    {
        super(message);
    }
}
