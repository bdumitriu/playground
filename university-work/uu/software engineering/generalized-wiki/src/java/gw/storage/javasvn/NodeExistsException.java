package gw.storage.javasvn;

/**
 * This exception indicates that you have tried to insert a node and another node with
 * the same path already existed in the tree.
 *
 * @author Bogdan Dumitriu
 */
public class NodeExistsException extends Exception
{
    public NodeExistsException()
    {
        super();
    }

    public NodeExistsException(String message)
    {
        super(message);
    }
}
