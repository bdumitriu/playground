package ass3.factory;

/**
 * Concrete Creator using SAX.
 *
 * @author bdumitriu
 *
 */
public class SaxDirBuilder extends DirBuilder {

	@Override
	public TreeProduct getTree(String file) {
		return new SaxTreeProduct(file);
	}
}
