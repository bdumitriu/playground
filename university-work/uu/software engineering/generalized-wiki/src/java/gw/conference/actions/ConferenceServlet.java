package gw.conference.actions;

import gw.GwServlet;
import gw.render.parsers.ParseException;
import gw.storage.StorageException;

import gw.conference.exceptions.ConferenceException;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.jdom.JDOMException;

/** 
 * A common superclass for any GW servlet, implementing doGet() and doPost()
 * with a call to the abstract method doRequest(ActionContext).
 */
public abstract class ConferenceServlet extends GwServlet {
	// TODO: Consider naming this class ApplicationServlet and moving it to the gw package
	
	public final void doPost(HttpServletRequest request, HttpServletResponse response)
    	throws ServletException, IOException {		
        
        doRequestHandlingExceptions(request, response, false);
	}
	
	public final void doGet(HttpServletRequest request, HttpServletResponse response)
		throws ServletException, IOException {    
        
        // Get requests always ignore the parameters given (out of security considerations)
        doRequestHandlingExceptions(request, response, true);
	}
    
    public void doRequestHandlingExceptions(HttpServletRequest request, HttpServletResponse response, boolean ignoreParameters) 
        throws ServletException, IOException {
        
        ActionContext action = new ActionContext(request, response, getSessionContext(request), ignoreParameters);  
        try {
            doRequest(action);
        } catch(ConferenceException x) {
            action.writeError(x);
        } catch(StorageException x) {
            action.writeError(x);
        } catch(Exception x) {
            action.writeError(x);
        } finally {
            action.close();
        }
    }
	
	public abstract void doRequest(ActionContext action) throws ConferenceException, StorageException, IOException, JDOMException, ParseException;
}
