package gw.actions;


import gw.GwServlet;
import gw.render.locator.StatusLocator;
import gw.render.locator.ResourceLocator;

import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;

/**
 * StatusFile displays the status for the requested file.
 * 
 * @author Michiel Overeem
 * @author Eric Bouwers
 */
public class StatusFile extends GwServlet {
	
	private static final ResourceLocator STATUS = new StatusLocator();

    /**
     * Presents the status overview of the path.
     * 
     * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {       
    	handleAction(request, response, STATUS);    	
    }

}