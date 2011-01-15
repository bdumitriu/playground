package ass3.factory;

/**
 * The Product interface.
 *
 * @author bdumitriu
 */
public interface TreeProduct {

	public String getName();

	public TreeProduct[] getChildren();

	public boolean isLeaf();
}
