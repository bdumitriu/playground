package gw.render;

import gw.GwContext;
import gw.GwSessionContext;

import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.util.HashMap;
import java.util.Map;

/**
 * StylesheetFactory is a factory to register and obtain stylesheets.
 * 
 * The stylesheets are looked up by content-type. This content-type is the
 * type of the stylesheet file, which must be in an InputStream. Typically,
 * this the content-type is text/xsl.
 */
public class StylesheetFactory {

    private Map<String, Constructor<? extends Stylesheet>> _stylesheets;

    public StylesheetFactory() {
        super();
        _stylesheets = new HashMap<String, Constructor<? extends Stylesheet>>();
    }

    /**
     * Registers Stylesheets by contenttype
     * 
     * @param contentType
     *            a String representing the contenttype
     * @param stylesheet
     *            the Class of the stylesheet that should be used for this
     *            contenttype. This class should implement a constructor with an
     *            inputStream and an URIResolver
     */
    public void register(String contentType, Class<? extends Stylesheet> stylesheet) {
        if (!Stylesheet.class.isAssignableFrom(stylesheet))
            throw new RegisterException("class does not implements the "
                    + Stylesheet.class.getName() + " interface");

        try {
            _stylesheets.put(contentType, stylesheet.getConstructor(new Class[] {
                    InputStream.class, GwSessionContext.class, GwContext.class }));
        } catch (NoSuchMethodException exc) {
            throw new RegisterException(stylesheet.getName()
                    + " does not have a constructor with arguments (InputStream, Storage)", exc);
        }
    }
    
    public Stylesheet newIdentity(GwContext ctx, GwSessionContext sessioncontext) {
        	return new IdentityStylesheet(sessioncontext);
    }

    /**
     * @param contentType
     *            A String representing the content-type of the stylesheet.
     * @param storage
     *            A Storage that can be used by this StyleSheet
     * @return A new stylesheet of the provided inputType, based on the
     *         inputStream
     * @throws StylesheetCreateException
     *             if anything goes wrong in the process of creating the
     *             StyleSheet
     */
    public Stylesheet newByType(String contentType, InputStream stylesheet, GwContext ctx,
            GwSessionContext sessioncontext) throws StylesheetCreateException {
    	    
        Constructor<? extends Stylesheet> constructor = _stylesheets.get(contentType);

        if (constructor == null) {
            ctx.log("No stylesheet class registered for content type " + contentType
                    + ". Falling back to identity stylesheet.");
            return newIdentity(ctx, sessioncontext);
        }

        try {
            return constructor.newInstance(new Object[] { stylesheet, sessioncontext, ctx});
        } catch (Exception e) {
            throw new StylesheetCreateException(e);
        }
    }
}
