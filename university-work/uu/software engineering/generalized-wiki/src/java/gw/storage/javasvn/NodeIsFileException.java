package gw.storage.javasvn;

/**
 * This exception indicates that you have tried to perform some operation on a file
 * which cannot be (logically) performed on a file (such as add a file or a directory
 * to it).
 * 
 * @author Bogdan Dumitriu
 */
public class NodeIsFileException extends Exception
{
    public NodeIsFileException()
    {
        super();
    }

    public NodeIsFileException(String message)
    {
        super(message);
    }
}
