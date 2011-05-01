package gw.util;

import java.util.regex.*;

/**
 * class used to extend standard XSLT 1.0 with regular expressions
 */
public class RegExp {

	/**
	 * replace the inputString with the first (matchGroup 1) Applies a replace
	 * regular expression on a certain input
	 * 
	 * @param input
	 *            The input string for matching
	 * @param regexp
	 *            regular expression
	 * @param replacement
	 *            replacement of the input
	 * @return the replacement following from the regular expression
	 */
	public static String replace(String input, String regexp, String replacement) {
		Pattern p = Pattern.compile(regexp);
		Matcher m = p.matcher(input);
		return m.replaceAll(replacement);
	}

	/**
	 * Applies a regular expression to the input, and returns the match with the
	 * specified matchGroup
	 * 
	 * @param input
	 *            The string on which the expression has to be applied
	 * @param regexp
	 *            The regular expression with for example (.*) for matching a
	 *            matchGroup
	 * @param matchGroup
	 *            The index of matchGroup to be returned
	 * @return
	 */
	public static String returnMatchedGroup(String input, String regexp,
			int matchGroup) {

		Pattern p = Pattern.compile(regexp);
		Matcher m = p.matcher(input);
		boolean succes = m.matches();
		if (succes) {
			return m.group(matchGroup);
		} else
			return "";

	}

	/*
	 * Example of usage (note the escaping for the compiler of the JAVA file AND
	 * the compiling of the regular expression )
	 * 
	 * System.out.println(returnMatchedGroup( "{\\sortunder{Beek} Prof. Dr. van
	 * Beek van Doornenbos}et al", "\\{\\\\sortunder\\{(.*)\\}(.*)\\}(.*)", 1));
	 * 
	 */

}
