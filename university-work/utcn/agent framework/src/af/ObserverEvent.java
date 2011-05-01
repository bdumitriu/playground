package af;

/**
 * A simple interface agents who want to use {@link ObserverBehaviour}s have to
 * implement.
 *
 * Date: May 22, 2003
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public interface ObserverEvent extends java.io.Serializable
{
	/**
	 * This method will be called periodically from the observer behaviour and it
	 * should return true when the event you are interested in has occured and false
	 * otherwise. When this method returns true, the {@link #callBackWhenOccured}
	 * method is called as a handler of this event.
	 *
	 * @return true when event has occured and false otherwise.
	 */
	public boolean hasOccured();

	/**
	 * This method is called by the observer behaviour when the {@link #hasOccured}
	 * method returns true. This method is seen as a handler of this type of event.
	 */
	public void callBackWhenOccured();
}
