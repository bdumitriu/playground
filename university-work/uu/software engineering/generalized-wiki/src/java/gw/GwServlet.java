package gw;

import gw.render.Stylesheet;
import gw.render.StylesheetApplyException;
import gw.render.StylesheetCreateException;
import gw.render.parsers.ParseException;
import gw.render.TransformationResource;
import gw.render.locator.ResourceLocator;
import gw.storage.*;

import java.io.*;
import java.util.*;

import javax.servlet.*;
import javax.servlet.http.*;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamResult;


/**
 * Implements the shared methods for the different possible actions in the wiki
 * system. This is the basis for the other servlets.
 * 
 * @author Eelco Visser
 * @author Michiel Overeem
 * @author Eric Bouwers
 * @author Jeroen Zuiderwijk
 * @author Patrick Camphuijsen
 * @author Ivaylo Gochkov
 */
public class GwServlet extends HttpServlet {
    
    private static GwContext _context;

    public void init() throws ServletException {
        if (_context == null) {
            _context = new GwContext(getServletContext());
        }
    }
    
    protected GwContext getGwContext() {
        return _context;
    }

    /**
     * Not implemented for HandleFile, all different servlets override this
     * function if needed.
     * 
     * @see javax.servlet.http.HttpServlet#doPost(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    public void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        print(response, GwConstants.XHTML_MIME_TYPE,
                getGwContext().getTextResources().getString("HandleFile.MethodError"), "<p>"
              + getGwContext().getTextResources().getString("HandleFile.MethodNotImplemented") + "</p>");
    }

    /**
     * Handles the GET requests. Not implemented for HandleFile, all different
     * servlets override this function if needed.
     * 
     * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        print(response, GwConstants.XHTML_MIME_TYPE, getGwContext().getTextResources().getString("HandleFile.MethodError"), "<p>"
                + getGwContext().getTextResources().getString("HandleFile.MethodNotImplemented") + "</p>");
    }

    /**
     * Fetches the session context from a request. The session context contains all
     * session-specific components and resources.
     * 
     * @param request The pertinent HttpServletRequest
     * @return The GwSessionContext
     * @throws SevletException
     * @see GwSessionContext
     */
    public GwSessionContext getSessionContext(HttpServletRequest request) throws ServletException {
        HttpSession session = request.getSession();
        GwSessionContext context = GwSessionContext.getContextFromSession(session);
        
        if (context == null) {
            GwSessionContext.installContextOnSession(session, _context);
            context = GwSessionContext.getContextFromSession(session);
        }
        
        if (context == null)
            throw new NullPointerException("Session context not initialized.");                
        
        context.setParameters(request.getParameterMap());
        
        return context;
    }

    /**
     * Prints some output to the ServletResponse.
     * 
     * @param response
     *            The HttpServletResponse to which we write
     * @param contentType
     *            The type of the response
     * @param title
     *            The title of this page
     * @param body
     *            The body of this page
     * @throws IOException
     */
    protected void print(HttpServletResponse response, String contentType, String title, String body)
            throws IOException {
        response.setContentType(contentType);
        PrintWriter out = response.getWriter();
        printHeader(out, title);
        out.println(body);
        printFooter(out);

    }

    /**
     * Prints the page header.
     *
     * @param out
     *            The PrintWriter in which the returned header can be printed
     * @param title
     *            The page title
     */
    public void printHeader(PrintWriter out, String title) {
      out.println("<html xmlns=\"http://www.w3.org/1999/xhtml\">");
      out.println("  <head>");
      // TODO: escape the title
      out.println("    <title>" + title + "</title>");
      out.println("    <meta http-equiv=\"cache-control\" content=\"no-cache\" />");
      out.println("    <meta http-equiv=\"pragma\" content=\"no-cache\" />");      
      out.println("  </head>");
      out.println("  <body>");
    }

    /**
     * Prints the page footer.
     * 
     * @param out
     *            The PrintWriter in which the returned footer can be printed
     */
    public void printFooter(PrintWriter out) {
      out.println("  </body>");
      out.println("</html>");      
    }

    /**
     * Prints a thrown exception in a readable layout.
     * 
     * @param response
     *            The response channel through which the output can be printed
     * @param exc
     *            The exception that was thrown
     * @throws IOException
     */
    protected void printException(HttpServletResponse response, Exception exc) throws IOException {
        response.setContentType(GwConstants.XHTML_MIME_TYPE);
        response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);

        PrintWriter out = new PrintWriter(new OutputStreamWriter(response.getOutputStream()));        
        out.println("<html xmlns=\"http://www.w3.org/1999/xhtml\">");
        out.println("<head>");
        out.println("<META http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/>");        
        out.println("<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\" href=\"/gw/styles/style.css\"/>");        
        out.println("<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\" href=\"/gw/styles/layout.css\"/>");        
        out.println("<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\" href=\"/gw/styles/gwml.css\"/>");        
        
        out.println("</head>");
        out.println("<body>");
        out.println("<div class=\"gwTopic\">");
        out.println("<div class=\"section\">");
        out.println("<div class=\"title\">");
        out.println("<div class=\"h1\">");
        out.println("<a name=\"N65543\">An error has occured inside GW</a>");
        out.println("</div>");
        out.println("</div>");
        out.println("<div class=\"para\">" + exc.getMessage() + "</div>");
        out.println("</div>");
        out.println("</div>");
        out.println("</body>");
        out.println("</html>");
        
        out.flush();
        out.close();
    }       
    

    /**
     * Creates a directory if it doesn't already exist.
     * 
     * @param storage
     *            The storage object in which the directory should be created
     * @param path
     *            The path that should be created
     * @throws StorageException
     */
    protected void createDirectory(Storage storage, String path) throws StorageException {
        String[] newDirs = ServletUtilities.splitPathInArray(path);
        String pathToCreate = "";

        for (int j = 0; j < newDirs.length; j++) {

            pathToCreate += "/" + newDirs[j];

            if (!storage.fileExists(pathToCreate)) {
                storage.makeDirectory(pathToCreate);
            }
        }
    }

    protected void registerChangedProperties(HttpServletRequest request, String pathInfo) {
        HttpSession session = request.getSession();
        ArrayList<String> changedProperties = (ArrayList<String>) session.getAttribute("propertyIsChanged");

        if (changedProperties == null) {
            changedProperties = new ArrayList<String>();
        }
        if (!changedProperties.contains(pathInfo)) {
            changedProperties.add(pathInfo);
        }

        session.setAttribute("propertyIsChanged", changedProperties);
    }
    
    /**
     * This handles the actions when a file exist
     * 
     * @throws IOException
     * @throws StorageException
     * @throws StylesheetApplyException
     * @throws StylesheetCreateException
     * 
     * @throws StylesheetApplyException
     * @throws StylesheetCreateException
     */    
    protected void handleAction(HttpServletRequest request, HttpServletResponse response, ResourceLocator locator) throws ServletException, IOException
    {    	
    	
    	
    	String pathInfo = ServletUtilities.getPathInfo(request);       
    	    	
    	try
    	{
    		pathInfo = ServletUtilities.normalizePathInfo(getSessionContext(request).getSessionStorage(), pathInfo);
    		
        	OutputStream output = response.getOutputStream();        	                	
        	
        	TransformationResource tResource = locator.locate(_context, getSessionContext(request), pathInfo);
        	Local.setSessionContext(getSessionContext(request));
        	Local.setGwContext(_context);
            
    		Stylesheet stylesheet = tResource.getStylesheet();
    		Source source = tResource.getSource();    		
    		Result result = new StreamResult(response.getOutputStream());
        		
            //apply the stylesheet
            stylesheet.apply(source, pathInfo, result);    	
    		 
            // set the response mime type of the http heade
        	response.setContentType(stylesheet.getMimeType(pathInfo));    	    	    
            
            output.flush();
            
    	} catch (InsufficientStorageAccessPermissionsException isape) {
	        ServletUtilities.setReferer(request);
	        getServletConfig().getServletContext().getRequestDispatcher("/login/").forward(request,
	                response);
	
	    } 
    	catch (StorageException se) {
	        printException(response, se);
	    } 
    	catch (StylesheetCreateException sce) {
	        printException(response, sce);
	    } 
    	catch (StylesheetApplyException sae) {
	        printException(response, sae);
	    } 
    	catch (ParseException sae) {
	        printException(response, sae);
	    }    	
    }
    
    /**
     * Returns a keyed value from the environment of the Servlet.
     * @param key The key.
     * @return The String value indexed by the key.
     */
    protected String getParamFromServlet( String key ) {
        return _context.getServletContext().getInitParameter( key );
    }
}
