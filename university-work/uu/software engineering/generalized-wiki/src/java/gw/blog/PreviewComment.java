package gw.blog;

import gw.GwConstants;
import gw.GwContext;
import gw.ServletUtilities;
import gw.render.ProxyStorage;
import gw.storage.StorageException;
import gw.users.StorageUserAliasDictionary;
import gw.users.acl.GwACLPermissions;
import gw.users.acl.StorageACLAdapter;

import java.util.Date;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;

import org.w3c.dom.Element;

/**
 * Servlet for creating and editing comments
 * 
 * @author Vincent Berkien
 *
 */

public class PreviewComment extends PreviewServlet {
	    
	/**
	 * Used to save the comment changes
	 */
	public void createVirtualFile(HttpServletRequest request, ProxyStorage proxyStorage, String user) 
	        throws XMLFactoryException, StorageException {

        storeComment(getGwContext(), proxyStorage, request, user);
	}
	
	/**
     * @param context
     *              The servlet context
	 * @param storage
	 * 				The Storage object
     * @param request
     *              The servlet request
	 * @param user
	 * 				User who posted the comment
	 */
	public void storeComment(GwContext context, ProxyStorage proxyStorage, HttpServletRequest request, String user)
            throws XMLFactoryException, StorageException {
        String pathInfo = ServletUtilities.getPathInfo(request);
        boolean fileExists = proxyStorage.fileExists(pathInfo);
        
		XMLFactory.createVirtualFile(proxyStorage, pathInfo, "<comment></comment>", GwConstants.BLOG_COMMENT_MIME_TYPE);
		
        XMLFactory factory = new XMLFactory(pathInfo, proxyStorage);
        storeCommentIntoFactory(context, factory, request, user);		
        factory.saveVirtualFile(GwConstants.BLOG_COMMENT_MIME_TYPE);
        
        if (!fileExists) {
            String entryAuthor = getAuthorOfEntry(proxyStorage, FileUtils.dirname(pathInfo) + "/entry.xml");
            setAuthorRights(proxyStorage, pathInfo, entryAuthor);
            if (!user.equals(entryAuthor)) {
                setUserRights(proxyStorage, pathInfo, user);
            }
        }
	}
	
	/**
	 * Write comment to XMLFactory
	 * 
     * @param context
     *              The servlet context
	 * @param factory
	 * 				XMLFactoryObject
     * @param request
     *              The servlet request
	 * @param user
	 * 				User who posted the comment
	 * 
	 */
	public void storeCommentIntoFactory(GwContext context, XMLFactory factory, HttpServletRequest request, String user)
	        throws XMLFactoryException {
        String content = request.getParameter("entry");
        String contentType = request.getParameter("contenttype");
        String date = XMLFactory.formatDate(new Date());
		
		Element commentElem = factory.getRoot();
		factory.addTextElemAndAppend("date", date, commentElem);
		factory.addTextElemAndAppend("author", user, commentElem);
        
		Element contentElem = factory.addElem("content", commentElem);
        contentElem.setAttribute("contenttype", contentType);
        contentElem.appendChild(factory.parseContent(context, content, contentType));	
	}
    
    private String getAuthorOfEntry(ProxyStorage proxyStorage, String pathInfo)
            throws StorageException, XMLFactoryException {
        XMLFactory xmlFactory = new XMLFactory(pathInfo, proxyStorage);
        Element root = xmlFactory.getRoot();
        Element author = xmlFactory.getFirstElementChildByName("author", root);
        return author.getTextContent();
    }
    
    private void setAuthorRights(ProxyStorage proxyStorage, String pathInfo, String author)
            throws StorageException {
        StorageACLAdapter aclAdapter = new StorageACLAdapter(pathInfo, proxyStorage);
        aclAdapter.addACLRight(GwACLPermissions.READ_PERMISSION, StorageUserAliasDictionary.DEFAULT_GROUP);
        aclAdapter.addACLRight(GwACLPermissions.WRITE_PERMISSION, author);
        aclAdapter.addACLRight(GwACLPermissions.DELETE_PERMISSION, author);
        aclAdapter.addACLRight(GwACLPermissions.BROWSE_PERMISSION, StorageUserAliasDictionary.DEFAULT_GROUP);
        aclAdapter.addACLRight(GwACLPermissions.PASSWORD_PERMISSION, author);
        aclAdapter.addACLRight(GwACLPermissions.ACL_WRITE_PERMISSION, author);
    }
    
    private void setUserRights(ProxyStorage proxyStorage, String pathInfo, String user)
            throws StorageException {
        StorageACLAdapter aclAdapter = new StorageACLAdapter(pathInfo, proxyStorage);
        aclAdapter.addACLRight(GwACLPermissions.WRITE_PERMISSION, user);
        aclAdapter.addACLRight(GwACLPermissions.DELETE_PERMISSION, user);
    }
    
}
