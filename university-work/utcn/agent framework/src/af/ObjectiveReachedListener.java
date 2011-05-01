package af;

/**
 *
 *
 * @author Tudor Marian
 * @author email: tudorm@coned.utcluj.ro
 * @version 0.1
 *
 * Date: Aug 6, 2003
 */
public interface ObjectiveReachedListener extends java.io.Serializable
{
	/**
	 * This method is called when an objective was reached in time.
	 *
	 * @param event
	 */
	public void callBackWhenReached(ObjectiveReachedEvent event);
}
