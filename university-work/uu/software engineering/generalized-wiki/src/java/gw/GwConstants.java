package gw;

/**
 * Collection of constant values used at various places in GW.
 * 
 * @author Martin Bravenboer
 */
public final class GwConstants {
	private GwConstants() {}
	
	public static final String CONTENT_TYPE_PROPERTY = "content-type";
	
    /** Mime Types */
    public static final String XSLT_MIME_TYPE  = "text/xsl"; // "application/xslt+xml";
    public static final String GWML_MIME_TYPE  = "application/gwml+xml";
    public static final String XHTML_MIME_TYPE = "application/xhtml+xml";
    public static final String BIBTEX_MIME_TYPE = "bibtex";
    public static final String BIBTEX_QUERY_MIME_TYPE = "bibtex-query"; 
    public static final String BLOG_INDEX_MIME_TYPE = "blog/index";
    public static final String BLOG_ENTRY_MIME_TYPE = "blog/entry";
    public static final String BLOG_COMMENT_MIME_TYPE = "blog/comment"; 
    public static final String CONFERENCE_RATINGS_MIME_TYPE = "conferencePaperRatings";
    public static final String XML_MIME_TYPE  = "text/xml";
    public static final String PLAIN_TEXT_MIME_TYPE  = "text/plain";
    
    /** Name of the file to display for directories. */
    public static final String INDEX_PAGE_NAME = "WebHome";
    
    public static final String GWML_ROOT_OPEN_TAG = "<document>";
    public static final String GWML_ROOT_CLOSE_TAG = "</document>";
    
    /** Constant that gives the key of the global subversion user. */
    public static final String SVN_USER     = "gw.svnuser";
    
    /** Constant that gives the key of the global subversion password. */
    public static final String SVN_PASSWORD     = "gw.svnpass";
    
    /** Constant that gives the key of the subversion repository where
        all Wiki data is saved. */
    public static final String SVN_REPOSITORY   = "gw.svnrepos";
    
    /** Constant that gives the key of the configuration file with the 
        validation regular expressions in it. */
    public static final String CONFIG_REGEX     = "gw.conf.regex";
}
