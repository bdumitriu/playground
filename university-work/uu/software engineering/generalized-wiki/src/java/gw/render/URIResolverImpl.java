package gw.render;

import gw.GwContext;
import gw.GwSessionContext;
import gw.ServletUtilities;
import gw.render.locator.ResolveFileLocator;
import gw.render.parsers.ParseException;
import gw.storage.StorageException;

import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URI;

import javax.xml.transform.Source;
import javax.xml.transform.TransformerException;
import javax.xml.transform.stream.StreamSource;
import javax.xml.transform.URIResolver;

import org.jdom.Element;
import org.jdom.transform.JDOMResult;
import org.jdom.transform.JDOMSource;

/**
 * The GW implementation of an URIResolver. This finds files or variables for
 * you.
 * 
 * FIXME: split this resolver and implement a resolver the combines a list of resolvers.
 */
public class URIResolverImpl implements URIResolver {

    private static final ResolveFileLocator locator = new ResolveFileLocator();
    private GwContext _context;
    private GwSessionContext _sessionContext;    

    public URIResolverImpl(GwSessionContext sessioncontext, GwContext context) {     
        _sessionContext = sessioncontext;
        _context = context;
    }

    /**
     * @return the resolved Source
     */
    public Source resolve(String href, String base) throws TransformerException {
        Source result;

        try {
            // FIXME: base all the processing on the URI.
            URI uri = new URI(href);
            String scheme = uri.getScheme();

            if (scheme.equals("resource")) {
                InputStream stream = getClass().getClassLoader().getResourceAsStream(
                        uri.getSchemeSpecificPart());
                if(stream == null) {
                    _context.log("ACLResource not available: " + href);
                    throw new TransformerException("ACLResource not available: " + href);
                }
                result = new StreamSource(stream);
            } else {
                Element element = null;

                if (scheme.equals("file")) {
                    element = getFileElement(uri);
                } else if(scheme.equals("wildcard")){
                    element = getFileElement(uri); 
                } else {
                    throw new TransformerException("Scheme " + scheme + " not implemented yet");
                }

                result = new JDOMSource(element);
            }
        } catch (TransformerException exc) {
            throw exc;
        } catch (Exception exc) {
            throw new TransformerException(exc);
        }

        return result;
    }

    private Element getFileElement(URI uri) throws StorageException,
             ParseException, StylesheetCreateException, StylesheetApplyException
    {
        return getFileElement(ServletUtilities.normalizePathInfo(_sessionContext.getSessionStorage(), uri.getPath()));
    }

    public Element getFileElement(String pathinfo) throws StorageException,
            ParseException, StylesheetCreateException, StylesheetApplyException {
    	
    	TransformationResource tr =  locator.locate(_context, _sessionContext, pathinfo);
    	    	
    	Stylesheet stylesheet = tr.getStylesheet();
    	JDOMSource source = (JDOMSource)tr.getSource();
    	JDOMResult result = new JDOMResult();
    	stylesheet.apply(source, pathinfo, result);    	    	    	
    	return (Element)result.getResult().get(0);
    }     
}