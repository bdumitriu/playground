package gw.blog;

import gw.GwConstants;
import gw.ServletUtilities;
import gw.actions.PreviewFile;
import gw.render.ProxyStorage;
import gw.storage.StorageException;

import gw.users.StorageUserAliasDictionary;
import gw.users.acl.StorageACLAdapter;
import gw.users.acl.GwACLPermissions;

import java.io.ByteArrayInputStream;
import java.util.HashMap;

import javax.servlet.http.HttpServletRequest;

import org.jdom.Element;
import org.jdom.output.XMLOutputter;

public class PreviewBlog extends PreviewServlet {
    
    public void createVirtualFile(HttpServletRequest request, ProxyStorage proxyStorage, String user) 
            throws StorageException {
        String pathInfo = ServletUtilities.getPathInfo(request);
        boolean fileExists = proxyStorage.fileExists(pathInfo); 
            
        Element result = processForm(request);
        
        XMLOutputter outputter = new XMLOutputter();
        String text = outputter.outputString(result);
        ByteArrayInputStream inStream = new ByteArrayInputStream(text.getBytes());
        
        HashMap<String, String> propertyMap = new HashMap<String, String>();
        propertyMap.put("content-type", GwConstants.BLOG_INDEX_MIME_TYPE);
        proxyStorage.addVirtualFile(pathInfo, inStream, propertyMap);
        
        if (!fileExists) {
            setRights(proxyStorage, pathInfo, user);
            setRights(proxyStorage, FileUtils.dirname(pathInfo), user);
        }
    }

    public Element processForm(HttpServletRequest request) {
        Element title = new Element("title");
        title.setText(request.getParameter("title"));

        Element description = new Element("description");
        description.addContent(request.getParameter("description"));

        Element blog = new Element("blog");
        blog.addContent(title);
        blog.addContent(description);

        return blog;
    }
    
    private void setRights(ProxyStorage proxyStorage, String pathInfo, String user)
            throws StorageException {
        StorageACLAdapter aclAdapter = new StorageACLAdapter(pathInfo, proxyStorage);
        aclAdapter.addACLRight(GwACLPermissions.READ_PERMISSION, StorageUserAliasDictionary.DEFAULT_GROUP);
        aclAdapter.addACLRight(GwACLPermissions.WRITE_PERMISSION, user);
        aclAdapter.addACLRight(GwACLPermissions.DELETE_PERMISSION, user);
        aclAdapter.addACLRight(GwACLPermissions.BROWSE_PERMISSION, StorageUserAliasDictionary.DEFAULT_GROUP);
        aclAdapter.addACLRight(GwACLPermissions.PASSWORD_PERMISSION, user);
        aclAdapter.addACLRight(GwACLPermissions.ACL_WRITE_PERMISSION, user);
    }
    
}
