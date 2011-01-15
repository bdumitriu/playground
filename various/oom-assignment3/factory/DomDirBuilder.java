package ass3.factory;

/**
 * Concrete Creator using DOM.
 *
 * @author bdumitriu
 *
 */
public class DomDirBuilder extends DirBuilder {

	@Override
	public TreeProduct getTree(String file) {
		return new DomTreeProduct(file);
	}
}
