package gw.util;

import gw.util.RegExp;

/**
 * Class used for sorting authornames in XSL Stylesheet formatting
 */
public class BibTex {

	/**
	 * @param toSort
	 *            node with author data
	 * @return toSort node with new added field to be able to sort in a better
	 *         way
	 */
	public static String processSortunder(String toSort) {
		String tmp;

		/*
		 * Only if the author node has the sortunder command, use the regular
		 * expression to extract the part of the name that has to be used for
		 * sorting.
		 */
		if (toSort.contains("sortunder")) {
			
			/*
			 * regular expression for extracting the sort name. the most of the
			 * backslashes are used for escaping during the compiling of the
			 * javafile.
			 */

			tmp = RegExp.returnMatchedGroup(toSort,
					"\\{\\\\sortunder\\{(.*)\\}(.*)\\}(.*)", 1);
			return tmp + toSort;
		} else {
			return toSort;
		}

	}
}
