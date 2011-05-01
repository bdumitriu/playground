package gw;

import gw.storage.Storage;
import gw.storage.StorageException;
import gw.users.acl.*;

import java.io.IOException;
import java.io.InputStream;
import java.util.*;

import javax.servlet.http.HttpServletRequest;
import org.jdom.Element;

/**
 * Contains all HTML forms for the different actions. All methods are static.
 * 
 * @author Michiel Overeem
 */
public class ServletForms {

    /**
     * Makes a form for setting the read/write properties of a file/directory.
     * 
     * @param acl
     *            The ACL definition from storage
     * @param permissions
     *            The current permissions
     * @param actionPath
     *            The directory or file to which these settings must apply
     * @return The form for setting the properties
     * @throws StorageException
     */
    public static String getACLForm(StorageACLAdapter acl, ACLPermission[] permissions,
            String actionPath, GwContext context) throws StorageException {

        String inputFields = "";

        for (int i = 0; i < permissions.length; i++) {
            String users = acl.getPermissionLine(permissions[i]);
            if (users == null) {
                users = "";
            }

            inputFields += permissions[i].getIdentifier() + ":" + "<input type=\"text\" name=\""
                    + permissions[i].getIdentifier() + "\" value=\"" + users + "\" /><br />\n";
        }

        String form = "<form id=\"aclForm\" action=\"" + actionPath + "\" method=\"post\">"
                + inputFields + "<input type=\"submit\" value=\""
                + context.getTextResources().getString("ACLFile.submit") + "\" />" + "</form>";
        return form;
    }


    /**
     * Returns a String with HTML that renders a form used for creating a user.
     * 
     * @param postTo
     *            set the action attribute of the form to this URL
     * @param warning
     *            boolean that indicates if warnings must be printed
     * @param warningResourceMsg
     *            resourceskeys from the warnings. Needs to be not null if
     *            warning is true
     * @param oldValues
     *            values that can be used to pre-set the form with values. Needs
     *            to be not null if warning is true
     * @return a String with HTML that renders a form used for creating a user.
     */
    public static String getCreateUserForm(String postTo, boolean warning, List warningResourceMsg,
            Map oldValues) {

        List localMsg = null;
        String userNameValue = "";
        String emailValue = "";

        // If warnings must be printed then warningsKeys must be available
        // and a map with old values.
        if (warning) {

            if (warningResourceMsg == null || oldValues == null) {
                throw new NullPointerException("Map or Resources are null, but a warning is needed");
            } else {
                localMsg = warningResourceMsg;
                if (oldValues.get("username") != null)
                    userNameValue = (String) oldValues.get("username");

                if (oldValues.get("email") != null)
                    emailValue = (String) oldValues.get("email");
            }
        } else {
            localMsg = new LinkedList();
        }
        // Print the HTML of two simple lables and two simple inputs
        //
        StringBuffer result = new StringBuffer();

        result.append("<form id=\"createUserForm\" action=\"" + postTo + "\" method=\"POST\" >");

        if (warning)
            for (int i = 0; i < localMsg.size(); ++i)
                result.append("<p>" + localMsg.get(i) + "</p>");

        result.append("<label for=\"usernameCtrl\" style=\"width: 80px;\">Username*:</label>");
        result.append("<input type=\"text\" id=\"usernameCtrl\" name=\"username\" value=\""
                + userNameValue + "\" />");
        result.append("<br />");
        result.append("<label for=\"emailCtrl\" style=\"width: 80px;\">E-mail*:</label>");
        result.append("<input type=\"text\" id=\"emailCtrl\" name=\"email\" value=\"" + emailValue
                + "\" />");
        result.append("<br />");
        result.append("<input type=\"submit\" />");
        result.append("</form>");
        return result.toString();
    }

    /**
     * Returns a String with HTML that renders a form that enables a user to
     * change his password.
     * 
     * @param postTo
     *            Path of the action for the form
     * @return a String with HTML that renders a form that enables a user to
     *         change his password.
     */
    public static String getSetPasswordForm(String postTo) {

        StringBuffer result = new StringBuffer();

        result.append("<form id=\"setPasswordForm\" action=\"" + postTo + "\" method=\"POST\" />");
        result.append("<label for=\"passwordCtrl0\">Old Password*</label>");
        result.append("<input type=\"password\" id=\"passwordCtrl0\" name=\"oldpassword\" />");
        result.append("<label for=\"passwordCtrl1\">New password*</label>");
        result.append("<input type=\"password\" id=\"passwordCtrl1\" name=\"newpassword\" />");
        result.append("<label for=\"passwordCtrl2\">Retype new password*</label>");
        result.append("<input type=\"password\" id=\"passwordCtrl2\" name=\"newpassword2\" />");
        result.append("<input type=\"submit\" /> ");
        result.append("</form> ");

        return result.toString();

    }

    /**
     * Returns the HTML form to confirm the deletion. It prints the appropiate
     * warning.
     * 
     * @param isDir
     *            True if the path is a directory, otherwise false
     * @param actionPath
     *            The actionpath of the form
     * @return The HTML form
     */
    public static String getDeleteForm(boolean isDir, String actionPath, GwContext context) {

        String warning = context.getTextResources().getString("Warning.DeleteFile");

        if (isDir) {
            warning = context.getTextResources().getString("Warning.DeleteDirectory");
        }

        return "<form id=\"frmDelete\" action=\"" + actionPath + "\" method=\"post\">\n" + warning
                + "<br/>\n" + "<input id=\"btnYes\" type=\"submit\" name=\"submit\" value=\""
                + context.getTextResources().getString("YesKey") + "\" />\n"
                + "<input id=\"btnNo\" type=\"submit\" name=\"submit\" value=\""
                + context.getTextResources().getString("NoKey") + "\" /></form>";

    }

    /**
     * Prints the form which enables the client to request a difference between
     * two revisions. It has two dropdown menus with possible revision numbers,
     * but with different parameter names.
     * 
     * @param actionPath
     *            The action path of the form
     * @param dropdown1
     *            The first dropdown menu with possible revision numbers
     * @param dropdown2
     *            The second dropdown menu with possible revision numbers
     * @param error
     *            The possible error that was encountered during the post
     * @return The form to request a difference between two revision
     */
    public static String getDiffForm(String actionPath, String dropdown1, String dropdown2,
            String error, GwContext context) {

        return error + "<form action=\"" + actionPath + "\" method = \"post\" >" + dropdown1
                + dropdown2 + "<input type=\"submit\" value=\""
                + context.getTextResources().getString("DiffFile.Submit") + "\" /></form>";

    }

    /**
     * Formats a drop down menu with all existing directories in the repository.
     * 
     * @param storage
     *            The Storage object to get all directories in the repository
     * @param selectedPath
     *            The path that has to be selected in the dropdown menu
     * @return The drop down menu formatted in HTML
     * @throws StorageException
     *             When Storage can not get a directory listing
     */
    public static String getDirListingSelect(Storage storage, String selectedPath)
            throws StorageException {

        StringBuffer result = new StringBuffer("<select name=\"newPathPart\">\n"
                + "<option></option>\n");

        Iterator dirs = storage.getDirListing("", true);

        while (dirs.hasNext()) {
            String path = (String) dirs.next();
            if (storage.isDirectory(path)) {
                if (!path.equals(selectedPath)) {
                    result.append("<option>" + path + "</option>\n");
                } else {
                    result.append("<option selected=\"selected\">" + path + "</option>\n");
                }
            }
        }

        result.append("</select>");

        return result.toString();

    }

    /**
     * Returns a HTML form for selecting a revision number. A simple dropdown
     * menu with all possible revision numbers.
     * 
     * @param actionPath
     *            The actionpath of the form
     * @param dropdown
     *            The dropdown with all possible revision numbers
     * @return The HTML form
     */
    public static String getHistoryForm(String actionPath, String dropdown, GwContext context) {

        return context.getTextResources().getString("HistoryFile.Of") + "<br />" + "<form action=\""
                + actionPath + "\" method = \"get\" >" + dropdown
                + "<input type=\"submit\" value=\""
                + context.getTextResources().getString("HistoryFile.Submit") + "\" /> </form>";

    }
     
    /**
     * Prints the form to display for selecting praises between different
     * revisions.
     * 
     * @param storage
     *            The SVNStorage object
     * @param contextPath
     *            The root of the filesystem
     * @param servletPath
     *            The path inside the servlet
     * @param path
     *            The path of the file
     * @param error
     *            Possible encountered error message
     * @return The form to display for selecting praises between different
     *         revisions
     * @throws IOException
     */
    public static String getPraiseForm(Storage storage, String contextPath, String servletPath,
            String path, String error, GwContext context) throws StorageException {

        String result = "";

        if (storage.fileExists(path)) {

            result = error
                    + "<br /><form action=\""
                    + contextPath
                    + servletPath
                    + path
                    + "\" method = \"get\" >"
                    + getRevisionNumbersSelect("from", ServletUtilities.getRevisionNumbers(storage,
                            path))
                    + getRevisionNumbersSelect("to", ServletUtilities.getRevisionNumbers(storage,
                            path)) + "<input type=\"submit\" value=\""
                    + context.getTextResources().getString("PraiseFile.Submit") + "\"/></form>";

        } else {
            result = context.getTextResources().getString("Error.FileNotFound");
        }

        return result;

    }
    
    /**
     * Prints the actual HTML form for the Get request. it looks for some
     * textresources that are specific for the class. So in the TextResources,
     * there should be values for the following keys: className.className,
     * className.Submit
     * 
     * @param actionpath
     *            The actionpath of the form
     * @param pathinfo
     *            The pathinfo of this request
     * @param dropdownmenu
     *            The dropdownmenu with existing paths
     * @param optionalInputs
     *            Optional input-fields. Inserted before the submit-button
     * @param className
     *            The class name of the caller class. This is used for the
     *            textresources.
     * @return The HTML form
     */
    public static String getReplaceForm(String actionpath, String pathinfo, String dropdownmenu,
            String optionalInputs, String className, GwContext context) {

        return context.getTextResources().getString(className + "." + className) + ": " + pathinfo + " "
                + context.getTextResources().getString("Text.To") + ":<br />\n"
                + "<form id=\"frmMove\" method=\"POST\" action=\"" + actionpath + "\">\n"
                + "<input type=\"hidden\" name=\"source\" value=\"" + pathinfo + "\" />"

                + "<table><tr><th>" + context.getTextResources().getString("CopyFileMoveFile.ExistingDirs")
                + "</th><th>" + context.getTextResources().getString("CopyFileMoveFile.NewDirs") + "</th><th>"
                + context.getTextResources().getString("CopyFileMoveFile.FileName") + "</th></tr>" + "<tr><td>"
                + dropdownmenu + "</td><td><input type=\"text\" name=\"target\" /></td>"
                + "<td><input type=\"text\" name=\"filename\" value=\""
                + ServletUtilities.getDirName(pathinfo) + "\"/>" + "</td></tr></table>"
                + context.getTextResources().getString("CopyFileMoveFile.ForceCreation")
                + ": <input type=\"checkbox\" name=\"forceCreation\" value=\"force\" /><br />"
                + optionalInputs
                + getSubmitButton("", context.getTextResources().getString(className + ".Submit"), "btnMove")
                + "</form>";

    }

    /**
     * Prints the HTML form for the user to provide a revision number to revert
     * to.
     * 
     * @param dropdown
     *            The HTML dropdown menu with all revisions
     * @param contextPath
     *            The Servlet Application name
     * @param servletPath
     *            The mapping name of this servlet
     * @param path
     *            The path from the request
     * @param error
     *            An optional error message in case an earlier request went
     *            wrong
     * @return The HTML form for the user to provide a revision number to revert
     *         to.
     * @throws IOException
     */
    public static String getRevertForm(String dropdown, String contextPath, String servletPath,
            String path, String error, GwContext context) {

        return error + "<br />" + context.getTextResources().getString("RevertFile.To") + "<br />"
                + "<form action=\"" + contextPath + servletPath + path + "\" method = \"post\" >"
                + dropdown + getSubmitButton("", context.getTextResources().getString("RevertFile.Submit"), "")
                + "</form>";

    }
    

    /**
     * Prints the HTML form for the user to specify which revision info he wants
     * to view.
     * 
     * @param actionPath
     *            The action path
     * @param dropdown
     *            The HTML dropdown menu with all revisions
     * @return The form for specifying the revision
     */
    public static String getRevisionForm(String actionPath, String dropdown, GwContext context) {

        return context.getTextResources().getString("RevisionInfo.AllLogs") + "<br />" + "<form action=\""
                + actionPath + "\" method=\"get\" >" + dropdown
                + getSubmitButton("", context.getTextResources().getString("RevisionInfo.Submit"), "")
                + "</form>";

    }

    /**
     * Formats an array of longs into an xHTML select box
     * 
     * @param parameterName
     *            The parameter name of the HMTL select-tag
     * @param revisionNumbers
     *            the array with revision numbers
     * @return The select box containing the revision numbers
     */
    public static String getRevisionNumbersSelect(String parameterName, long[] revisionNumbers) {

        StringBuffer select = new StringBuffer("");

        for (int i = 0; i < revisionNumbers.length; i++) {
            select.insert(0, "<option>" + revisionNumbers[i] + "</option>");
        }
        select.insert(0, "<select name=\"" + parameterName + "\">" + "<option> </option>");
        select.append("</select>");

        return select.toString();
    }

    /**
     * Returns a HTML submitButton with the given parameters as parameters of
     * the button. Uses the method getInput to make the button.
     * 
     * @param name
     *            The name of the button. This one parameter may be empty.
     * @param value
     *            The value of the textfield, the string that is seen on the
     *            button This parameter may not be empty
     * @param id
     *            The id of the submitbutton. this one is used for testing the
     *            application through httpunit
     * @return The string with the submitbutton.
     */
    private static String getSubmitButton(String name, String value, String id) {
        return getInput("submit", name, value, id);
    }

    /**
     * The generalized method used to create HTML for input things such as
     * submitbuttons, text or checkboxes.
     * 
     * @param type
     *            The type that is given to the HTML input.
     * @param name
     *            The name that is given to the HTML input.
     * @param value
     *            The value that is given to the HTML input.
     * @param id
     *            The id that is given to the HTML input.
     * @return The string which holds the HTML input.
     */
    private static String getInput(String type, String name, String value, String id) {
        return "<input type=\"" + type + "\" id=\"" + id + "\" name=\"" + name + "\" value=\""
                + value + "\" />";
    }

    /**
     * Generates a form were a user can fill in it's username and password and
     * is posted to a given location.
     * 
     * @param sendTo
     *            String representation of the URL where the form is posted to.
     * @param messages
     *            A List with Strings that will be displayed above the form.
     * @return String with HTML that renders the form.
     */
    public static String getSendNewPasswordForm(String sendTo, List messages) {
        String output;

        if (messages.isEmpty())
            output = getCreateUserForm(sendTo, false, null, null);
        else
            output = getCreateUserForm(sendTo, true, messages, new HashMap());

        return output;
    }
}
