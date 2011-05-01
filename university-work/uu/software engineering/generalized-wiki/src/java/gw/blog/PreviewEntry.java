package gw.blog;

import gw.GwConstants;
import gw.GwContext;
import gw.ServletUtilities;
import gw.render.ProxyStorage;
import gw.storage.StorageException;

import java.util.Date;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;

import org.w3c.dom.Element;

import gw.users.StorageUserAliasDictionary;
import gw.users.acl.StorageACLAdapter;
import gw.users.acl.GwACLPermissions;

/** 
 * Servlet that allows users to create and edit an entries in a blog.   
 * 
 * @author Chris van Dam
 * @author Vincent Berkien
 * 
 */

public class PreviewEntry extends PreviewServlet {

    /**
     * Used to save the blog entry that was edited or created.
     */
    public void createVirtualFile(HttpServletRequest request, ProxyStorage proxyStorage, String user) 
            throws XMLFactoryException, StorageException {

        storeEntry(getGwContext(), proxyStorage, request, user);
    }

    /**
     * Stores an entry to a virtual entry file.
     * 
     * @param context
     *              The servlet context
     * @param proxyStorage
     *              The proxyStorage objest
     * @param request
     *              The servlet request
     * @param user
     *              The user that posted/edited the entry
     */
    
    public void storeEntry(GwContext context, ProxyStorage proxyStorage, HttpServletRequest request, String user)
            throws XMLFactoryException, StorageException {
        String pathInfo = ServletUtilities.getPathInfo(request);
        boolean fileExists = proxyStorage.fileExists(pathInfo);
        
        XMLFactory.createVirtualFile(proxyStorage, pathInfo, "<entry></entry>", GwConstants.BLOG_ENTRY_MIME_TYPE);
        
        XMLFactory factory = new XMLFactory(pathInfo, proxyStorage);
        storeEntryInFactory(context, factory, request, user);
        factory.saveVirtualFile(GwConstants.BLOG_ENTRY_MIME_TYPE);
        
        if (!fileExists) {
            setFileRights(proxyStorage, pathInfo, user);
            setDirectoryRights(proxyStorage, FileUtils.dirname(pathInfo));
        }
        
        boolean draft = "yes".equals(request.getParameter("draft"));
        setReadRights(proxyStorage, pathInfo, user, draft);
    }
    
    /**
     * Stores an entry to a factory
     * 
     * @param context
     *              The servlet context
     * @param factory
     *              The XML Factory
     * @param request
     *              The servlet request
     * @param user
     *              The user that posted/edited the entry
     */
    public void storeEntryInFactory(GwContext context, XMLFactory factory, HttpServletRequest request, String user)
            throws XMLFactoryException {
        String title = request.getParameter("title");
        String content = request.getParameter("entry");
        String contentType = request.getParameter("contenttype"); 
        String date = XMLFactory.formatDate(new Date());
        
        String draft = request.getParameter("draft");
        if (draft == null) {
            draft = "no";
        }
        
        Element root = factory.getRoot();
        factory.addAttribute("draft", draft, root);
        factory.addTextElemAndAppend("title",title,root);
        factory.addTextElemAndAppend("date",date,root);
        factory.addTextElemAndAppend("author",user,root);
        
        Element contentElem = factory.addElem("content", root);
        contentElem.setAttribute("contenttype", contentType);
        contentElem.appendChild(factory.parseContent(context, content, contentType));
    }
    
    private void setFileRights(ProxyStorage proxyStorage, String pathInfo, String user)
            throws StorageException {
        StorageACLAdapter aclAdapter = new StorageACLAdapter(pathInfo, proxyStorage);
        aclAdapter.addACLRight(GwACLPermissions.WRITE_PERMISSION, user);
        aclAdapter.addACLRight(GwACLPermissions.DELETE_PERMISSION, user);
        aclAdapter.addACLRight(GwACLPermissions.PASSWORD_PERMISSION, user);
        aclAdapter.addACLRight(GwACLPermissions.ACL_WRITE_PERMISSION, user);
    }
    
    private void setDirectoryRights(ProxyStorage proxyStorage, String pathInfo)
            throws StorageException {
        StorageACLAdapter aclAdapter = new StorageACLAdapter(pathInfo, proxyStorage);
        aclAdapter.addACLRight(GwACLPermissions.READ_PERMISSION, StorageUserAliasDictionary.DEFAULT_GROUP);
        aclAdapter.addACLRight(GwACLPermissions.WRITE_PERMISSION, StorageUserAliasDictionary.DEFAULT_GROUP);
        aclAdapter.addACLRight(GwACLPermissions.DELETE_PERMISSION, StorageUserAliasDictionary.DEFAULT_GROUP);
        aclAdapter.addACLRight(GwACLPermissions.BROWSE_PERMISSION, StorageUserAliasDictionary.DEFAULT_GROUP);
        aclAdapter.addACLRight(GwACLPermissions.PASSWORD_PERMISSION, StorageUserAliasDictionary.DEFAULT_GROUP);
        aclAdapter.addACLRight(GwACLPermissions.ACL_WRITE_PERMISSION, StorageUserAliasDictionary.DEFAULT_GROUP);
    }

    private void setReadRights(ProxyStorage proxyStorage, String pathInfo, String user, boolean draft)
            throws StorageException {
        String userId = draft ? user : StorageUserAliasDictionary.DEFAULT_GROUP;
        
        StorageACLAdapter aclAdapter = new StorageACLAdapter(pathInfo, proxyStorage);
        aclAdapter.addACLRight(GwACLPermissions.READ_PERMISSION, userId);
        aclAdapter.addACLRight(GwACLPermissions.BROWSE_PERMISSION, userId);
    }


}

