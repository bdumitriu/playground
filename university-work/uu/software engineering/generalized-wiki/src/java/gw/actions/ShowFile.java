package gw.actions;

import gw.GwServlet;

import gw.render.locator.ViewLocator;
import gw.render.locator.ResourceLocator;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
/**
 * Shows the rendered contents of a file or directory.
 */
public class ShowFile extends GwServlet {
	
	private static final ResourceLocator VIEW = new ViewLocator(); 

    /**
     * Shows the rendered contents of a file.
     * 
     * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
    	handleAction(request, response, VIEW);
    }
}