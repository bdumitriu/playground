package gw.users.actions;

import java.io.*;
import java.util.*;
import javax.servlet.*;
import javax.servlet.http.*;
import gw.*;
import gw.storage.*;
import gw.users.*;
import gw.util.DigestAuthenticationUtility;
import gw.util.SmtpMailer;

/**
 * Creates a new user.
 */
public class UserCreateServlet extends GwServlet {

    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        print(response, GwConstants.XHTML_MIME_TYPE, "Create new User", ServletForms
                .getCreateUserForm(request.getContextPath() + request.getServletPath(), false,
                        null, null));
    }

    public void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        String userId = request.getParameter("username");
        String email = request.getParameter("email");

        Map values = new HashMap();
        values.put("username", userId);
        values.put("email", email);
        List messages = new ArrayList();

        if (userId == null || userId.trim().equals("")) {

            messages.add("The given username can not be empty");
            values.remove("username");
        }

        if (email == null || email.trim().equals("")) {

            messages.add("The given email address can not be empty");
            values.remove("email");
        }

        if (messages.isEmpty()) {

            // initialize the storage object
            GwSessionContext gsc = getSessionContext(request);

            // Use unsecure storage here, because we want to add the user in
            // (group) files
            // that may or may not be secured.
            //
            Storage storage = gsc.getUnsecuredStorage();
            UserDataAdapter uda = UserDataAdapterFactory.getUserDataAdapter(storage);
            DataAdapterUserManagerFactory daumf = new DataAdapterUserManagerFactory(uda);
            UserManager userManager = daumf.getUserManager();

            String password = DigestAuthenticationUtility.getRandomPassword(userId);

            userManager.createUser(userId, email, password);

            try {
            	System.out.println("password is " + password);
                SmtpMailer mailer = new SmtpMailer(getParamFromServlet("gw.emailsmtp"), email,
                        getParamFromServlet("gw.emailfrom"), "Welcome to the GW as user", getNewUserMail(userId, password));
                mailer.send();
            } catch (IOException ioEx) {

                // User is created, but there is a problem with mailing the
                // user. Uhm
                // just delete for now then.
                //
                uda.delete(new User(userId));
            }

            messages.add("User succesfully added to the Wiki");
            values.clear();
        }

        print(response, GwConstants.XHTML_MIME_TYPE, "Create new User", ServletForms
                .getCreateUserForm(request.getContextPath() + request.getServletPath(), true,
                        messages, values));

    }
    
    /**
     * Returns the text for the email to the new user.
     * @param username The username.
     * @param password The password.
     * @return The text for the message.
     */
	private String getNewUserMail( String username, String password ) {		
		StringBuffer output 	= new StringBuffer( "Dear " + username + ",\n");
		output.append( "\n");
		output.append( "Welcome to the exiting world of the General Wiki project.\n");
		output.append( "You login/password data for this Wiki is " + username + " / " + password );
		return output.toString();
	}
}
