package org.ffplanner.controller;

import org.ffplanner.bean.UserBean;
import org.ffplanner.controller.auth.AuthData;
import org.ffplanner.controller.auth.RegistrationService;
import org.ffplanner.entity.User;
import org.ffplanner.entity.UserRole;
import org.ffplanner.qualifier.LoggedInUser;
import org.ffplanner.util.*;
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

    private static final String MY_OPENID_URL = "https://myopenid.com/"; //NON-NLS

    private DiscoveryInformation discoveryInformation;

    private AuthData authData;

    @Inject
    private UserBean userBean;

    @Produces @LoggedInUser
    private User user;

    public boolean isAdminUser() {
        return user != null && user.hasRole(UserRole.ADMIN);
    }

    public void logOut() {
        final FacesContext facesContext = FacesContext.getCurrentInstance();
        final ExternalContext context = facesContext.getExternalContext();
        ((HttpSession) context.getSession(false)).invalidate();
        try {
            context.redirect(FacesUtils.getUrlToRoot(facesContext));
        } catch (IOException e) {
            Logging.getInstance().log(logger, "Redirect failed: ", e);
        }
    }

    public void logInWithGoogle() {
        logIn(GOOGLE_OPENID_URL);
    }

    public void logInWithYahoo() {
        logIn(YAHOO_OPENID_URL);
    }

    public void logInWithMyOpenId() {
        logIn(MY_OPENID_URL);
    }

    private void logIn(String identifier) {
        discoveryInformation = RegistrationService.performDiscoveryOnUserSuppliedIdentifier(identifier);
        final String returnToUrl = RegistrationService.getReturnToUrl();
        final AuthRequest authRequest = RegistrationService.createOpenIdAuthRequest(discoveryInformation, returnToUrl);
        try {
            FacesContext.getCurrentInstance().getExternalContext().redirect(authRequest.getDestinationUrl(true));
        } catch (IOException e) {
            Logging.getInstance().log(logger, "Redirect failed: ", e);
        }
    }

    public void processOpenIDProviderResponse() {
        if (user == null) {
            final FacesContext facesContext = FacesContext.getCurrentInstance();
            final ExternalContext externalContext = facesContext.getExternalContext();
            final Map<String, String> parameterMap = externalContext.getRequestParameterMap();
            final AuthData returnData = RegistrationService.processReturn(
                    discoveryInformation, parameterMap, RegistrationService.getReturnToUrl());
            if (returnData != null) {
                authData = returnData;
            }
            if (authData == null) {
                if (!facesContext.getResponseComplete()) {
                    JsfViews.LOGIN_TARGET.redirectTo("#{auth.processOpenIDProviderResponse}");
                }
            } else {
                final User user = userBean.findBy(authData.getOpenId());
                if (user != null) {
                    authData = null;
                    setUser(user);
                    redirectToUserRequestedPage();
                }
            }
        }
    }

    public String createNewAccount() {
        if (user == null && authData != null) {
            user = userBean.createWith(authData);
            authData = null;
            redirectToUserRequestedPage();
            return null;
        } else {
            return null;
        }
    }

    private static void redirectToUserRequestedPage() {
        final FacesContext facesContext = FacesContext.getCurrentInstance();
        final ExternalContext externalContext = facesContext.getExternalContext();
        try {
            if (!facesContext.getResponseComplete()) {
                final String redirectPath = (String) externalContext.getSessionMap().get("redirectTo");
                if (redirectPath != null) {
                    externalContext.redirect(redirectPath);
                } else {
                    externalContext.redirect(FacesUtils.getUrlToRoot(facesContext));
                }
            }
        } catch (IOException e) {
            Logging.getInstance().log(logger, "Redirect failed: ", e);
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
