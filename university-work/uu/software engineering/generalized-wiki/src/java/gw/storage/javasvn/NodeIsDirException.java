package gw.storage.javasvn;

/**
 * This exception indicates that you have tried to perform some operation on a
 * directory which cannot be (logically) performed on a directory (such as write its
 * contents).
 * 
 * @author Bogdan Dumitriu
 */
public class NodeIsDirException extends Exception
{
    public NodeIsDirException()
    {
        super();
    }

    public NodeIsDirException(String message)
    {
        super(message);
    }
}
