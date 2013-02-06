package org.ffplanner.controller;

import org.ffplanner.bean.UserBean;
import org.ffplanner.controller.auth.AuthData;
import org.ffplanner.controller.auth.RegistrationService;
import org.ffplanner.entity.User;
import org.ffplanner.qualifier.LoggedInUser;
import org.ffplanner.util.Logging;
import org.openid4java.discovery.DiscoveryInformation;
import org.openid4java.message.AuthRequest;

import javax.enterprise.context.SessionScoped;
import javax.enterprise.inject.Produces;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.inject.Inject;
import javax.inject.Named;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.io.Serializable;
import java.util.Map;
import java.util.logging.Logger;

/**
 * @author Bogdan Dumitriu
 */
@Named(value = "auth")
@SessionScoped
public class AuthController implements Serializable {

    private static final long serialVersionUID = 1L;

    private static final Logger logger = Logger.getLogger(AuthController.class.getName());

    private static final String GOOGLE_OPENID_URL = "https://www.google.com/accounts/o8/id"; //NON-NLS

    private static final String YAHOO_OPENID_URL = "https://me.yahoo.com/"; //NON-NLS

    private String redirectPath;

    private DiscoveryInformation discoveryInformation;

    private AuthData authData;

    @Inject
    private UserBean userBean;

    @Produces @LoggedInUser
    private User user;

    public void logOut() {
        final ExternalContext context = FacesContext.getCurrentInstance().getExternalContext();
        ((HttpSession) context.getSession(false)).invalidate();
        try {
            context.redirect(context.getRequestContextPath());
        } catch (IOException e) {
            Logging.getInstance().log(logger, "Redirect failed: ", e);
        }
    }

    public void logInWithGoogle(String redirectPath) {
        logIn(GOOGLE_OPENID_URL, redirectPath);
    }

    public void logInWithYahoo(String redirectPath) {
        logIn(YAHOO_OPENID_URL, redirectPath);
    }

    private void logIn(String identifier, String redirectPath) {
        discoveryInformation = RegistrationService.performDiscoveryOnUserSuppliedIdentifier(identifier);
        this.redirectPath = redirectPath;
        final String returnToUrl = RegistrationService.getReturnToUrl();
        final AuthRequest authRequest = RegistrationService.createOpenIdAuthRequest(discoveryInformation, returnToUrl);
        try {
            FacesContext.getCurrentInstance().getExternalContext().redirect(authRequest.getDestinationUrl(true));
        } catch (IOException e) {
            Logging.getInstance().log(logger, "Redirect failed: ", e);
        }
    }

    public void processOpenIDProviderResponse() {
        if (user == null && authData == null) {
            final ExternalContext externalContext = FacesContext.getCurrentInstance().getExternalContext();
            final Map<String, String> parameterMap = externalContext.getRequestParameterMap();
            authData = RegistrationService.processReturn(
                    discoveryInformation, parameterMap, RegistrationService.getReturnToUrl());
            if (authData == null) {
                try {
                    if (!FacesContext.getCurrentInstance().getResponseComplete()) {
                        externalContext.redirect("Login.xhtml");
                    }
                } catch (IOException e) {
                    Logging.getInstance().log(logger, "Redirect failed: ", e);
                }
            } else {
                final User user = userBean.findBy(authData.getOpenId());
                if (user != null) {
                    authData = null;
                    setUser(user);
                    try {
                        if (!FacesContext.getCurrentInstance().getResponseComplete()) {
                            if (redirectPath != null) {
                                externalContext.redirect(redirectPath);
                            } else {
                                externalContext.redirect("auth/DaySchedule.xhtml");
                            }
                        }
                    } catch (IOException e) {
                        Logging.getInstance().log(logger, "Redirect failed: ", e);
                    }
                }
            }
        }
    }

    public String createNewAccount() {
        if (user == null && authData != null) {
            user = userBean.createWith(authData);
            authData = null;
            if (redirectPath != null) {
                try {
                    FacesContext.getCurrentInstance().getExternalContext().redirect(redirectPath);
                } catch (IOException e) {
                    Logging.getInstance().log(logger, "Redirect failed: ", e);
                }
                return null;
            } else {
                return "/auth/DaySchedule";
            }
        } else {
            return null;
        }
    }

    public AuthData getAuthData() {
        return authData;
    }

    public void setAuthData(AuthData authData) {
        this.authData = authData;
    }

    public User getUser() {
        return user;
    }

    public void setUser(User user) {
        this.user = user;
    }
}
