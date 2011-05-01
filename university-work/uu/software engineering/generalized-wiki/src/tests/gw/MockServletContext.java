package gw;

import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Enumeration;
import java.util.Set;

import javax.servlet.RequestDispatcher;
import javax.servlet.Servlet;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;

import com.mockobjects.MockObject;

import gw.GwConstants;

/**
 * Mocks a ServletContext, implementing only the methods needed to test the GwContext.
 * 
 * TODO: This is really ugly, but necessary while GwContext is not an interface we can mock..
 */
public class MockServletContext extends MockObject implements ServletContext {

	public Object getAttribute(String arg0) {
		notImplemented();
		return null;
	}

	public Enumeration getAttributeNames() {
		notImplemented();
		return null;
	}

	public ServletContext getContext(String arg0) {
		notImplemented();
		return null;
	}

	/**
	 * Return specific strings when necessary to facilitate testing
	 * 
	 * TODO: remove this when GwContext has been made an interface.
	 */
	public String getInitParameter(String key) {
	    if(key == GwConstants.SVN_USER)
	    	return "gwuser";
	    if(key == GwConstants.SVN_USER)
	    	return "gwpassword";
	    if(key == GwConstants.SVN_REPOSITORY)
	    	return "svn://localhost:10000/";
		return "";
	}

	public Enumeration getInitParameterNames() {
		notImplemented();
		return null;
	}

	public int getMajorVersion() {
		notImplemented();
		return 0;
	}

	public String getMimeType(String arg0) {
		notImplemented();
		return null;
	}

	public int getMinorVersion() {
		notImplemented();
		return 0;
	}

	public RequestDispatcher getNamedDispatcher(String arg0) {
		notImplemented();
		return null;
	}

	public String getRealPath(String arg0) {
		notImplemented();
		return null;
	}

	public RequestDispatcher getRequestDispatcher(String arg0) {
		notImplemented();
		return null;
	}

	public URL getResource(String arg0) throws MalformedURLException {
		notImplemented();
		return null;
	}

	public InputStream getResourceAsStream(String arg0) {
		notImplemented();
		return null;
	}

	public Set getResourcePaths(String arg0) {
		notImplemented();
		return null;
	}

	public String getServerInfo() {
		notImplemented();
		return null;
	}

	public Servlet getServlet(String arg0) throws ServletException {
		notImplemented();
		return null;
	}

	public String getServletContextName() {
		notImplemented();
		return null;
	}

	public Enumeration getServletNames() {
		notImplemented();
		return null;
	}

	public Enumeration getServlets() {
		notImplemented();
		return null;
	}

	public void log(Exception arg0, String arg1) {
		notImplemented();
		
	}

	public void log(String arg0, Throwable arg1) {
		notImplemented();
	}

	/**
	 * Do nothing.
	 */
	public void log(String arg0) {
		
	}

	public void removeAttribute(String arg0) {
		notImplemented();
	}

	public void setAttribute(String arg0, Object arg1) {
		notImplemented();
	}
}
