package gw.actions;

import gw.GwServlet;

import gw.render.locator.RevisionLocator;
import gw.render.locator.ResourceLocator;

import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;

/**
 * Provides a servlet for viewing commits and log-messages. By default it shows
 * the last commit and when giving a get parameter "revision" it shows from that
 * revision to the head all commits.
 * 
 * @author Patrick Camphuijsen
 * @author Jeroen Zuijderwijk
 * @author Michiel Overeem
 */
public class RevisionInfo extends GwServlet {
	
	private static final ResourceLocator REVISION = new RevisionLocator();

    /**
     * Shows the commit messages and information.
     * 
     * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
    	
    	handleAction(request, response, REVISION);
    }
}