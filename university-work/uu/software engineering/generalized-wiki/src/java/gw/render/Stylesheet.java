package gw.render;

import javax.xml.transform.Source;
import javax.xml.transform.Result;

/**
 * Interface for a Stylesheet
 * 
 * TODO: support a specific revision.
 */
public interface Stylesheet {
	
    /**
     * Retreives the mime type from the xslt stylesheet.
     * 
     * @param pathinfo
     *            The path of the file on which the Stylesheet should be applied
     * @param user
     *            The user to which the file belongs
     */
	public String getMimeType(String pathinfo);



	/**
	 * @param source the xml to be transformed; could be something like 'new JDOMSource(element)'
	 * @param user the user logged in and requesting the transformation
	 * @param output the result of the transformation
	 * @throws StylesheetApplyException
	 */
	public void apply(Source source, String pathinfo, Result result) throws StylesheetApplyException;	
}
