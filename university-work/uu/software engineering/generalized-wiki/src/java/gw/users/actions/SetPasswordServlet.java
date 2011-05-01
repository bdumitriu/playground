package gw.users.actions;

import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;
import gw.*;
import gw.storage.*;
import gw.users.*;
import gw.util.DigestAuthenticationUtility;
import gw.util.SmtpMailer;

/**
 * Changes the password of a user.
 */
public class SetPasswordServlet extends GwServlet {

    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
    	GwSessionContext gsc = getSessionContext(request);

        print(response, GwConstants.XHTML_MIME_TYPE, "Set password for " + gsc.getOwner(), ServletForms
                .getSetPasswordForm(request.getContextPath() + request.getServletPath()));
    }

    public void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        GwSessionContext gsc = getSessionContext(request);

        Storage storage = gsc.getUnsecuredStorage();
        UserDataAdapter uda = UserDataAdapterFactory.getUserDataAdapter(storage);
        DataAdapterUserManagerFactory daumf = new DataAdapterUserManagerFactory(uda);
        UserManager userManager = daumf.getUserManager();
        PrintWriter out = response.getWriter();

        String oldPassword = request.getParameter("oldpassword");
        String newPassword = request.getParameter("newpassword");
        String newPassword2 = request.getParameter("newpassword2");

        /*
         * Check for the old password!
         */

        if ((oldPassword == null) || (newPassword == null) || (newPassword2 == null)) {
            out.println("Error: missing information");
        } else {
            if (newPassword.equals(newPassword2)) {
                try {
                    User user = gsc.getOwner();
                    if (User.ANONYMOUS_USER == user) {
                        out.println("Anonymous user's password cannot be changed.");
                    } else {
                        String passDigest = DigestAuthenticationUtility.getPasswordHash(user
                                .getId(), oldPassword);
                        String realPassDigest = uda.getPassword(user);

                        if (realPassDigest.equals(passDigest)) {
                            userManager.setPassword(user, newPassword);
                            try {
                                // Only
                                // commits by
                                // hand may
                                // happen
                                //
                                // storage.commit( "Changed password for user "
                                // + user.getId() );
                                out.println("Password changed successfully.");

                                SmtpMailer mailer = new SmtpMailer(getInitParameter("gw.emailsmtp"), user
                                        .getEmail(), getInitParameter("gw.emailfrom"), "New GW password",
                                        getSetPassMail(user.getId(), newPassword));
                                mailer.send();
                                out.println("It was sent by email to you.");
                            }
                            /*
                             * catch (StorageException e) { throw new
                             * IllegalStateException( e.toString() ); }
                             */
                            catch (IOException e) {
                                throw new IllegalStateException(e.toString());
                            }
                        } else {
                            out.println("Old password is incorrect.");
                        }
                    }
                } catch (IllegalStateException e) {
                    throw new RuntimeException("Should not happen.");
                }
            } else {
                out.println("Error: passwords do not match");
            }
        }
    }
    
	/**
	 * Returns the text for a modified password email.
	 * @param username The username.
	 * @param password The password.
	 * @return The text for a modified password email.
	 */
	private String getSetPassMail( String username, String password ) {	
		StringBuffer output		= new StringBuffer( "Dear " + username + ",\n" );
		output.append( "\n" );
		output.append( "The modification of your password was taken in consideration.\n" );
		output.append( "Your new password is: " + password );
		return output.toString();
	}
}
