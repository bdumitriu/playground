package gw;

import gw.render.ParserFactory;
import gw.render.StylesheetFactory;
import gw.render.parsers.*;
import gw.render.XSLTStylesheet;

import java.util.ResourceBundle;
import javax.servlet.ServletContext;

/**
 * The Context of GW provides access to components and resources.
 * 
 * @author Martin Bravenboer
 */
public class GwContext {
    
    /**
     * Contains all text related resources for the wiki system.
     */
    private ResourceBundle _textresources;
    
    /**
     * Contains all configuration related resources for the wiki system.
     */
    private ResourceBundle _configresources;

    private ServletContext _servletCtx;
    private StylesheetFactory _stylesheetFactory;
    private ParserFactory _parserFactory;
   
    public GwContext(ServletContext servletCtx) {
        super();
        _servletCtx = servletCtx;            
    }
    
    /**
     * Returns the StylesheetFactory for this GwContext.
     */
    public synchronized StylesheetFactory getStylesheetFactory() {
        if(_stylesheetFactory == null) {
            _stylesheetFactory = new StylesheetFactory();
            _stylesheetFactory.register(GwConstants.XSLT_MIME_TYPE, XSLTStylesheet.class);            
        }
        
        return _stylesheetFactory;     
    }
    
    public synchronized ParserFactory getParserFactory() {
        if(_parserFactory == null) {
       	    _parserFactory = new ParserFactory();
            _parserFactory.register(GwConstants.GWML_MIME_TYPE,GwConstants.XML_MIME_TYPE, XMLParser.getInstance());
            _parserFactory.register(GwConstants.BIBTEX_MIME_TYPE,GwConstants.XML_MIME_TYPE, BibParser.getInstance());
            _parserFactory.register(GwConstants.BIBTEX_QUERY_MIME_TYPE, GwConstants.XML_MIME_TYPE, BibQueryParser.getInstance());
            _parserFactory.register(GwConstants.XHTML_MIME_TYPE, GwConstants.XML_MIME_TYPE, XMLParser.getInstance());            
            _parserFactory.register(GwConstants.XSLT_MIME_TYPE, GwConstants.XML_MIME_TYPE, XMLParser.getInstance()); 
            _parserFactory.register(GwConstants.BLOG_INDEX_MIME_TYPE, GwConstants.XML_MIME_TYPE, XMLParser.getInstance());            
            _parserFactory.register(GwConstants.BLOG_ENTRY_MIME_TYPE, GwConstants.XML_MIME_TYPE, XMLParser.getInstance());            
            _parserFactory.register(GwConstants.BLOG_COMMENT_MIME_TYPE, GwConstants.XML_MIME_TYPE, XMLParser.getInstance());
            _parserFactory.register(GwConstants.CONFERENCE_RATINGS_MIME_TYPE, GwConstants.XML_MIME_TYPE, XMLParser.getInstance());
            
            _parserFactory.register(GwConstants.BIBTEX_MIME_TYPE, GwConstants.PLAIN_TEXT_MIME_TYPE, BibToPlainTextParser.getInstance()); 
        }        
        return _parserFactory;
    }

    /**
     * Returns the TextResources object.
     * 
     * @return the TextResourceBundle
     */
    public synchronized ResourceBundle getTextResources() {
        if(_textresources == null) {
            _textresources = ResourceBundle.getBundle("gw.TextResources");            
        }
      
        return _textresources;
    }

    /**
     * Returns the ConfigResources object.
     * 
     * @return the ConfigResourceBundle
     */
    public synchronized ResourceBundle getConfigResources() {
        if(_configresources == null) {
            _configresources = ResourceBundle.getBundle("gw.ConfigResources");
        }
        
        return _configresources;
    }
       
    public String getApplicationPrefix() {
    	 String applicationPrefix = "";
         if(_servletCtx != null) {
        	 applicationPrefix = _servletCtx.getInitParameter("applicationPrefix");
         }
		return applicationPrefix;
	}

	public void log(String msg) {
        if(_servletCtx != null) {
            _servletCtx.log(msg);
        }
        else {
            System.err.println(msg);
        }
    }
    
    public void log(String msg, Throwable throwable) {
        if(_servletCtx != null) {
            _servletCtx.log(msg, throwable);            
        }
        else {        
            System.err.println(msg);
            throwable.printStackTrace(System.err);
        }
    }
    
    public ServletContext getServletContext() {
        return _servletCtx;
    }
}
