package gw.users.actions;

import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;
import gw.*;
import gw.storage.*;
import gw.users.*;

/**
 * Servlet that clears the current user from the session.
 * 
 * @author John van Schie
 */
public class LogoutServlet extends GwServlet {
    
    /**
     * Remove the current logged in user from the session.
     */
	public void doGet  ( HttpServletRequest request, HttpServletResponse response )
											throws ServletException, IOException {
	
		handleLogOut(request, response);					
	}
   
	/**
	 * Remove the current logged in user from the session.
	 */ 
    public void doPost  ( HttpServletRequest request, HttpServletResponse response )
        									throws ServletException, IOException {

		handleLogOut(request, response);	
    }
    
    private void handleLogOut( HttpServletRequest request, HttpServletResponse response ) 
											throws ServletException, IOException {
    	GwSessionContext gsc = getSessionContext(request);
        
		// Logout the user and redirect him to the view of the trunk
		gsc.getSam().logout();
		response.sendRedirect( request.getContextPath()+"/view/" );	
    }
}
