package ass3.factory;

import java.io.File;

/**
 * The Creator.
 *
 * @author bdumitriu
 */
public abstract class DirBuilder {

	/**
	 * Creates a directory structure based on an XML file.
	 * 
	 * @param file
	 *            the XML file to read
	 * @param inDir
	 *            the directory in which to build the directory structure
	 */
	public void createDirectories(String file, String inDir) {
		createDirectories(new File(inDir), getTree(file));
	}

	public void createDirectories(File pathSoFar, TreeProduct product) {
		File dir = new File(pathSoFar, product.getName());
		dir.mkdir();
		if (!product.isLeaf()) {
			for (TreeProduct childProduct : product.getChildren()) {
				createDirectories(dir, childProduct);
			}
		}
	}

	/**
	 * The factory method.
	 *
	 * @return an instance of a class that implements the TreeProduct interface
	 */
	public abstract TreeProduct getTree(String file);
}
