package af;

/**
 * This interface has to be implemented by the user of the client / server
 * behaviour. The user has to define a class implementing this interface both
 * on the client and the server side and pass it to the message sending routines.
 *
 *
 * User: tudorm
 * Date: Aug 21, 2003
 * Time: 1:43:23 PM
 * To change this template use Options | File Templates.
 */
public interface AFClientServerJob
{
	/**
	 * This method will be called by the agent framework at both ends of the communication
	 * when the request / reply arrives. If the <code>commObject</code> is null it means
	 * that some sort of error occured during the communication process.
	 * The return value should be null on the client side and should return the object
	 * to send back to the client on the server side.
	 * <br /><br />
	 * Warning: this method is run in a multithreaded environment and should be thread-safe.
	 *
	 * @param commObject the object received from the client on the server
	 * side and from the server on the client side.
	 */
	AFCommObject process(AFCommObject commObject);
}
