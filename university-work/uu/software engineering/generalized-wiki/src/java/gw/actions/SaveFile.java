package gw.actions;

import gw.GwServlet;
import gw.ServletUtilities;
import gw.storage.*;

import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;

import gw.users.acl.GwACLPermissions;
import gw.users.acl.StorageACLAdapter;

/**
 * Saves posted text to the given path. Text is posted from PreviewFile and
 * saved here. It redirects to the viewing of this path or to the commit
 * servlet.
 * 
 * @author Eelco Visser
 * @author Jeroen Zuijderwijk
 * @author Eric Bouwers
 */
public class SaveFile extends GwServlet {

    /**
     * Saves the posted text.
     * 
     * @see javax.servlet.http.HttpServlet#doPost(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    public void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        Storage storage = getSessionContext(request).getSessionStorage();

        String pathInfo = ServletUtilities.getPathInfo(request);

        try {
            // look if it isn't a directory
            if (!storage.isDirectory(pathInfo)) {
                saveInput(pathInfo, storage, request);
            }

            // check if the user want to commit and redirect
            if (request.getParameter("commit") == null) {
                response.sendRedirect(request.getContextPath() + "/view" + pathInfo);
            } else {
                response.sendRedirect(request.getContextPath() + "/commit" + pathInfo);
            }

        } catch (InsufficientStorageAccessPermissionsException isape) {
            ServletUtilities.setReferer(request);
            getServletConfig().getServletContext().getRequestDispatcher("/login/").forward(request,
                    response);
        } catch (StorageException se) {
            printException(response, se);
        }

    }

    /**
     * Saves the actuall input without further testing of the path.
     * 
     * @param pathInfo
     *            The path of the file to which we want to save
     * @param storage
     *            The storage object
     * @param request
     *            The HttpServletRequest
     * @throws StorageException
     * @throws IOException
     */
    private void saveInput(String pathInfo, Storage storage, HttpServletRequest request)
            throws StorageException, IOException {

        OutputStream ostream;

        ostream = storage.storeFile(pathInfo);

        OutputStreamWriter out = new OutputStreamWriter(ostream);
        String text = request.getParameter("text");
        String contentType = request.getParameter("contenttype");
       // System.out.println("[SaveFile.saveInput(...)] content-type: "+ contentType);
        //if(contentType.equals("bibtex"))
        	//text = "<Entries><string></string><list><tuple><Entry><string>InProceedings</string><string>DV02-csmr</string><list><Field><string>author</string><Words><list><string>Arie</string><string>van</string><string>Deursen</string><string>and</string><string>Eelco</string><string>Visser</string></list></Words></Field><Field><string>title</string><Words><list><string>The</string><string>Reengineering</string><string>Wiki</string></list></Words></Field><Field><string>booktitle</string><Words><list><string>Proceedings</string><string>6th</string><string>European</string><string>Conference</string><string>on</string><string>Software</string><string>Maintenance</string><string>and</string><string>Reengineering</string><string>(CSMR)</string></list></Words></Field><Field><string>year</string><Words><list><string>2002</string></list></Words></Field><Field><string>pages</string><QWords><list><string>217--220</string></list></QWords></Field><Field><string>publisher</string><Words><list><string>IEEE</string><string>Computer</string><string>Society</string></list></Words></Field><Field><string>url</string><Words><list><string>http://www.program-transformation.org/re/</string></list></Words></Field><Field><string>urlinfo</string><Words><list><string>http://www.program-transformation.org/Transform/ReengineeringWikiPaper</string></list></Words></Field><Field><string>urlpdf</string><Words><list><string>http://www.cs.uu.nl/people/visser/ftp/DV02-csmr.pdf</string></list></Words></Field><Field><string>pubcat</string><Words><list><string>conference</string></list></Words></Field></list><NoComma/></Entry><string></string></tuple></list><string></string></Entries>";

        if (text == null) {
            text = "";
        }

        out.write(text);
        out.close();

        boolean contentTypeChanged = !contentType.equals(storage.getProperties(pathInfo).get("content-type"));
        storage.setProperty(pathInfo, "content-type", contentType, false);
        
        String acl = request.getParameter("acl");
        boolean permissionsChanged = false;
        if (acl != null) {
            permissionsChanged = setPermissions(pathInfo, storage, acl);
        }

        if (contentTypeChanged || permissionsChanged) {
            registerChangedProperties(request, pathInfo);
        }
    }
    
    /**
     * Set permissions on a file.
     * 
     * @param pathInfo
     *          The path of the file on wich we want to set permissions
     * @param storage
     *          The storage object
     * @param acl
     *          The permissions to set, seperated by colons
     * @return
     *          True if permissions have changed, false otherwise. 
     * @throws StorageException
     */
    private boolean setPermissions(String pathInfo, Storage storage, String acl)
            throws StorageException {
        StorageACLAdapter aclStorage = new StorageACLAdapter(pathInfo, storage);
        boolean changed = false;
        
        String[] aclArray = acl.split(":", GwACLPermissions.ALL_PERMISSIONS.length);
        for (int i = 0; i < aclArray.length; i++) {
            String permission = aclArray[i];
            if (!permission.equals("")) {
                String oldPermission = aclStorage.getPermissionLine(GwACLPermissions.ALL_PERMISSIONS[i]);
                if (!permission.equals(oldPermission)) {
                    changed = true;
                }
                aclStorage.setPermissionLine(GwACLPermissions.ALL_PERMISSIONS[i], permission);
            }
        }
        return changed;
    }

}
