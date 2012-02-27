/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.controller;

import org.ffplanner.controller.auth.RegistrationModel;
import org.ffplanner.controller.auth.RegistrationService;
import org.openid4java.discovery.DiscoveryInformation;
import org.openid4java.message.AuthRequest;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import javax.faces.context.FacesContext;
import java.io.IOException;
import java.io.Serializable;
import java.util.Map;
import java.util.logging.Logger;

/**
 * @author Bogdan Dumitriu
 */
@ManagedBean
@SessionScoped
public class AuthController implements Serializable {

    private static final Logger log = Logger.getLogger(AuthController.class.getName());

    private String userSuppliedIdentifier;

    private DiscoveryInformation discoveryInformation;

    private RegistrationModel registrationModel;

    public void logIn() {
        discoveryInformation = RegistrationService.performDiscoveryOnUserSuppliedIdentifier(userSuppliedIdentifier);
        final AuthRequest authRequest =
                RegistrationService.createOpenIdAuthRequest(discoveryInformation, RegistrationService.getReturnToUrl());
        try {
            FacesContext.getCurrentInstance().getExternalContext().redirect(authRequest.getDestinationUrl(true));
        } catch (IOException e) {
            final String message = "Redirect failed: " + e.getMessage();
            log.severe(message);
            throw new RuntimeException(message, e);
        }
    }

    public void processOpenIDProviderResponse() {
        if (registrationModel == null) {
            final Map<String, String> parameterMap =
                    FacesContext.getCurrentInstance().getExternalContext().getRequestParameterMap();
            registrationModel = RegistrationService.processReturn(
                    discoveryInformation, parameterMap, RegistrationService.getReturnToUrl());
        }
    }

    public String getUserSuppliedIdentifier() {
        return userSuppliedIdentifier;
    }

    public void setUserSuppliedIdentifier(String userSuppliedIdentifier) {
        this.userSuppliedIdentifier = userSuppliedIdentifier;
    }

    public RegistrationModel getRegistrationModel() {
        return registrationModel;
    }

    public void setRegistrationModel(RegistrationModel registrationModel) {
        this.registrationModel = registrationModel;
    }
}
