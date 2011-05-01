package gw.blog;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import gw.GwServlet;
import gw.GwSessionContext;
import gw.render.ProxyStorage;
import gw.render.locator.PreviewLocator;
import gw.render.locator.ResourceLocator;
import gw.storage.Storage;
import gw.storage.StorageException;
import gw.users.acl.GwACLPermissions;
import gw.users.acl.StorageACLAdapter;
import gw.users.StorageUserAliasDictionary;

public abstract class PreviewServlet extends GwServlet {
    
    private static final ResourceLocator PREVIEW = new PreviewLocator();

    public void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        Storage storage = getSessionContext(request).getSessionStorage();
        
        ProxyStorage proxyStorage = new ProxyStorage(storage);
        GwSessionContext sessionContext = getSessionContext(request); 
        sessionContext.setSessionStorage(proxyStorage);

        String user = getUserId(request);
        try {
            createVirtualFile(request, proxyStorage, user);           
            handleAction(request, response, PREVIEW);  
        } catch (XMLFactoryException xfe) {
            printException(response, xfe);
        } catch (StorageException se) {
            printException(response, se);
        } finally {
            sessionContext.setSessionStorage(storage);          
        }
    }
    
    protected abstract void createVirtualFile(HttpServletRequest request, ProxyStorage proxyStorage, String user)
            throws XMLFactoryException, StorageException;
    
    private String getUserId(HttpServletRequest request) {
        String user = "";
        try {
            user = getSessionContext(request).getOwner().getId();
        } catch (ServletException se) {
            se.printStackTrace();
        }
        return user;
    }
    
}
