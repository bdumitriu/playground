package gw.users.actions;

import javax.servlet.http.*;
import javax.servlet.ServletException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import gw.GwServlet;
import gw.GwConstants;
import gw.ServletForms;
import gw.GwSessionContext;

import gw.users.*;
import gw.util.DigestAuthenticationUtility;
import gw.util.SmtpMailer;

import gw.storage.SessionStorageFactory;
import gw.storage.Storage;

/**
 * Generates a random password for the user.
 */
public class GeneratePasswordServlet extends GwServlet {

    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        print(response, GwConstants.XHTML_MIME_TYPE, "Create new User", ServletForms.getSendNewPasswordForm(request
                .getContextPath()
                + request.getServletPath(), new ArrayList()));
    }

    public void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        // initialize Storage object to get the unsecure storage from it
        //
        GwSessionContext gsc = getSessionContext(request);
        Storage storage = gsc.getUnsecuredStorage();

        // Get the email adress form the user and compare it to the given email
        // address
        //
        UserDataAdapter uda = UserDataAdapterFactory.getUserDataAdapter(storage);
        User requestedUser = new User((String) request.getParameter("username"));
        uda.load(requestedUser);

        String email = ((String) request.getParameter("email")).trim();
        List messages = new ArrayList();

        if (requestedUser.getEmail().equals(email)) {

            String password = DigestAuthenticationUtility.getRandomPassword(requestedUser.getId());

            SmtpMailer mailer = new SmtpMailer(getInitParameter("gw.emailsmtp"), email, 
                    getInitParameter("gw.emailfrom"), "Your new GW Password", getNewPassMail(requestedUser.getId(), password));
            try {

                mailer.send();
                uda.setPassword(requestedUser, password);
                messages
                        .add("An Email with a new generated password is send to the email address of the user");
            } catch (IOException ioEx) {

                messages.add("There are problems with sending mail. Please try later again");
            }
        } else {

            messages.add("Incorrect UserId/Email combination");
        }

        print(response, GwConstants.XHTML_MIME_TYPE, "Create new User", ServletForms.getSendNewPasswordForm(request
                .getContextPath()
                + request.getServletPath(), messages));
    }
    
    /**
     * Returns the text for a password email.
     * @param username The username.
     * @param password The password.
     * @return The text for a password email.
     */
	private String getNewPassMail( String username, String password ) {
		
		StringBuffer output 	= new StringBuffer( "Dear " + username + ",\n");
		output.append( "\n");
		output.append( "A new password was requested by someone.\n");
		output.append( "You new password this Wiki is " + password );
		return output.toString();
	}
}
