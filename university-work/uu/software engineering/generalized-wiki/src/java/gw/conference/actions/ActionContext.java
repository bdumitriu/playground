package gw.conference.actions;

import gw.GwSessionContext;
import gw.ServletUtilities;
import gw.conference.Conference;
import gw.conference.ConferenceFactory;
import gw.conference.Paper;
import gw.conference.PaperFactory;
import gw.conference.exceptions.InvalidConferenceException;
import gw.conference.exceptions.InvalidPaperException;
import gw.storage.Storage;
import gw.storage.StorageException;
import gw.users.User;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public final class ActionContext {
    private final HttpServletRequest _request;
    private final HttpServletResponse _response;
    private final GwSessionContext _sessionContext;
    private final Map<String,String> _parameters;
    private final User _user;
    private final String _contextPath;
    private final String _pathInfo;
    private final Storage _storage;
    private final PrintWriter _out;
    
    public ActionContext(HttpServletRequest request, HttpServletResponse response, GwSessionContext sessionContext, boolean ignoreParameters) 
        throws IOException, ServletException {
        
        _request = request;
        _response = response;
        _sessionContext = sessionContext;
        
        _contextPath = request.getContextPath();
        _pathInfo = ServletUtilities.getPathInfo(request); // empty pathInfo is rewritten to "/"
        // Path gets validated by ConferenceManager
        
        // set headers before accessing the Writer
        response.setContentType("text/html");
        response.setBufferSize(8192);
        _out = response.getWriter();
        
        _storage = sessionContext.getSessionStorage();
        _user = sessionContext.getOwner();

        // Reads out all the parameters from the form, and prints them.
        if(ignoreParameters) _parameters = new HashMap<String,String>(); 
        else _parameters = getValidatedFields(request, request.getParameterMap());
    }

    /*
     * ACCESSORS
     */
    public GwSessionContext getSessionContext() {
        return _sessionContext;
    }
    public Map<String,String> getParameters() {
        return _parameters;
    }
    public User getUser() {
        return _user;
    }
    public String getContextPath() {
        return _contextPath;
    }
    /** Returns the path as given to this servlet. */
    public String getPathInfo() {
        return _pathInfo;
    }
    public Storage getStorage() {
        return _storage;
    }
    public Storage getUnsecuredStorage() {
        return _sessionContext.getUnsecuredStorage();
    }
    
    /*
     * PUBLIC METHODS
     */
    
    public Conference openConferenceHere() throws StorageException, InvalidConferenceException {
        return ConferenceFactory.openConference(getStorage(), getContextPath(), getPathInfo());
    }
    
    public Paper openPaperHere() throws StorageException, InvalidConferenceException, InvalidPaperException {
        return PaperFactory.openPaper(getStorage(), getContextPath(), getPathInfo());
    }
    
    /** Redirects the user to view a page in the current GW */
    public void sendViewRedirect(String localGwPath) throws IOException {
        // TODO: Don't hardcode "/view" here
        _response.sendRedirect(_request.getContextPath() + "/view" + localGwPath);        
    }
    
    /** 
     * Redirects the user to view the current pathInfo. 
     * @see #getPathInfo
     */
    public void sendViewRedirect() throws IOException {
        sendViewRedirect(_pathInfo);      
    }
    
    /** Writes a value to the associated PrintWriter. */ 
    public void write(String s) {
        _out.write(s);
    }
    
    public void writeGwml(String s) {
        // TODO: Convert Gwml using style shits; see view servlet
        write(s);
    }
    
    public void close() {
        _out.close();
    }
    
    public void writeMessage(String s) {
        write("<html><body><h1>Request complete</h1><p>" + s + "</p></body></html>");
    }
    
    public void writeError(String s) {
        write("<html><body><h1>Error</h1><p>" + s + "</p></body></html>");
    }
    
    public void writeError(Exception x) {
        writeError(x.toString());
    }
    
    /*
     * HELPER METHODS
     */
    
    
    private static Map<String,String> getValidatedFields(HttpServletRequest request, Map parameters) {
        Map<String,String> validated = new HashMap<String,String>();        
      
        for(Object param : parameters.keySet()) {           
            String key  = param.toString();
            String value = request.getParameter(key.toString()); 
            
            if(isValidField(key) && isValidField(value))
                validated.put(key, value);
        }    
        
        return validated;
    }
    
    private static boolean isValidField(String toCheck) {
        // TODO: isValidField(); remove tags and special characters
        // (was not a priority this year)
        return true;
    }    
}
