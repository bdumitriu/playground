package org.ffplanner.controller;

import org.ffplanner.entity.User;
import org.ffplanner.qualifier.LoggedInUser;
import org.ffplanner.util.FacesUtils;
import org.ffplanner.util.Logging;

import javax.enterprise.context.RequestScoped;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.inject.Inject;
import javax.inject.Named;
import java.io.IOException;
import java.util.logging.Logger;

/**
 * @author Bogdan Dumitriu
 */
@RequestScoped
@Named
public class LoginController {

    private static final Logger logger = Logger.getLogger(LoginController.class.getName());

    @Inject
    @LoggedInUser
    private User user;

    public void processOpenIDProviderResponse() {
        final FacesContext facesContext = FacesContext.getCurrentInstance();
        if (user != null && !facesContext.getResponseComplete()) {
            final ExternalContext externalContext = facesContext.getExternalContext();
            try {
                externalContext.redirect(FacesUtils.getUrlToRoot(facesContext));
            } catch (IOException e) {
                Logging.getInstance().log(logger, "Redirect failed: ", e);
            }
        }
    }
}
