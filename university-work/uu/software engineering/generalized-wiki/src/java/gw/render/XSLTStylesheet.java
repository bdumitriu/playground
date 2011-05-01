package gw.render;

import gw.GwContext;
import gw.GwSessionContext;

import java.io.InputStream;

import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;

import javax.xml.transform.stream.StreamSource;

/**
 * XSLTStyleSheet is the implementation of StyleSheet for xslt stylesheets
 */
public class XSLTStylesheet implements Stylesheet {

	private Transformer _transformer;

	public XSLTStylesheet(InputStream stylesheet, GwSessionContext sessioncontext, GwContext context)
			throws StylesheetCreateException {
		
		TransformerFactory transformerFactory = TransformerFactory.newInstance();
		URIResolverImpl resolver = new URIResolverImpl(sessioncontext, context);
		transformerFactory.setURIResolver(resolver);

		try {       			
			_transformer = transformerFactory.newTransformer(new StreamSource(stylesheet));
		} 
		catch (TransformerException exc) {
			throw new StylesheetCreateException(exc.getLocalizedMessage(),exc.getCause());
		}
	}
	
    public String getMimeType(String pathinfo)
    {
    	return _transformer.getOutputProperties().getProperty("media-type");
    }

	
	/**
	 * @param source the xml to be transformed; could be something like 'new JDOMSource(element)'
	 * @param user the user logged in and requesting the transformation
	 * @param output the result of the transformation
	 * @throws StylesheetApplyException
	 */
	public void apply(Source source, String pathinfo, Result result)
	throws StylesheetApplyException {		
		
		_transformer.setParameter("pathinfo", pathinfo);        
		try
		{                       
			_transformer.transform(source, result);			
		}
		catch (Exception exc)
		{
			throw new StylesheetApplyException(exc);
		}
		finally
		{
		    _transformer.clearParameters();
        }
	}
}
