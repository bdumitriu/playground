package gw.actions;

import gw.GwConstants;
import gw.GwServlet;
import gw.ServletForms;
import gw.ServletUtilities;
import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import gw.storage.*;
import gw.users.acl.*;

/**
 * Makes an interface for the changing of the ACL lists.
 * 
 * @author Eric Bouwers
 */
public class ACLFile extends GwServlet {

    /**
     * Shows the rendered contents of a file.
     * 
     * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        String pathname = ServletUtilities.getPathInfo(request);
        Storage storage = getSessionContext(request).getSessionStorage();

        String actionPath = ServletUtilities.getActionPath(request);

        StorageACLAdapter acl = new StorageACLAdapter(pathname, storage);

        ACLPermission[] permissions = GwACLPermissions.ALL_PERMISSIONS;

        try {
            String ACLForm = ServletForms.getACLForm(acl, permissions, actionPath, getGwContext());
            print(response, GwConstants.XHTML_MIME_TYPE, getGwContext().getTextResources().getString("ACLFile.title"), ACLForm);
        } catch (InsufficientStorageAccessPermissionsException isape) {
            ServletUtilities.setReferer(request);
            getServletConfig().getServletContext().getRequestDispatcher("/login/").forward(request,
                    response);

        } catch (StorageException se) {
            printException(response, se);
        }
    }

    public void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        String pathname = ServletUtilities.getPathInfo(request);
        Storage storage = getSessionContext(request).getSessionStorage();

        StorageACLAdapter acl = new StorageACLAdapter(pathname, storage);
        ACLPermission[] permissions = GwACLPermissions.ALL_PERMISSIONS;
        boolean propertyChanged = false;

        try {
            for (int i = 0; i < permissions.length; i++) {
                String acls = acl.getPermissionLine(permissions[i]);
                String newAcls = request.getParameter(permissions[i].getIdentifier());
                if (acls != null && !acls.equals(newAcls)) {
                    propertyChanged = true;
                }
                acl.setPermissionLine(permissions[i], newAcls);
            }
        } catch (StorageException se) {
            printException(response, se);
        }

        if (propertyChanged) {
            registerChangedProperties(request, pathname);
        }

        String redirectPath = ServletUtilities.getActionPath(request);
        response.sendRedirect(redirectPath);
    }

}
