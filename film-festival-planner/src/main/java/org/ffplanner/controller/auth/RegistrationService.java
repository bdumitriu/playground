/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.controller.auth;

import org.ffplanner.util.Logging;
import org.openid4java.association.AssociationException;
import org.openid4java.consumer.*;
import org.openid4java.discovery.DiscoveryException;
import org.openid4java.discovery.DiscoveryInformation;
import org.openid4java.discovery.Identifier;
import org.openid4java.message.*;
import org.openid4java.message.ax.AxMessage;
import org.openid4java.message.ax.FetchRequest;
import org.openid4java.message.ax.FetchResponse;

import javax.faces.context.FacesContext;
import javax.servlet.ServletRequest;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

/**
 * @author Bogdan Dumitriu
 */
public class RegistrationService {

    private static final Logger logger = Logger.getLogger(RegistrationService.class.getName());

    private static ConsumerManager consumerManager;

    /**
     * Perform discovery on the User-Supplied identifier and return the DiscoveryInformation object that results from
     * Association with the OP. This will probably be needed by the caller (stored in Session perhaps?).
     */
    @SuppressWarnings("unchecked")
    public static DiscoveryInformation performDiscoveryOnUserSuppliedIdentifier(String identifier) {
        try {
            final ConsumerManager consumerManager = getConsumerManager();
            final List<DiscoveryInformation> discoveries = consumerManager.discover(identifier);
            return consumerManager.associate(discoveries);
        } catch (DiscoveryException e) {
            Logging.getInstance().log(logger, "Error occurred during discovery: ", e);
            return null;
        }
    }

    /**
     * Create an OpenID Auth Request, using the DiscoveryInformation object return by the openid4java library.
     * <p />
     * This method also uses the Simple Registration Extension to grant the Relying Party (RP).
     */
    public static AuthRequest createOpenIdAuthRequest(DiscoveryInformation discoveryInformation, String returnToUrl) {
        try {
            final AuthRequest authRequest = getConsumerManager().authenticate(discoveryInformation, returnToUrl);
            final FetchRequest fetchRequest = FetchRequest.createFetchRequest();
            fetchRequest.addAttribute("email", "http://axschema.org/contact/email", true);
            fetchRequest.addAttribute("firstName", "http://axschema.org/namePerson/first", true);
            fetchRequest.addAttribute("lastName", "http://axschema.org/namePerson/last", true);
            fetchRequest.addAttribute("fullname", "http://axschema.org/namePerson", true);
            authRequest.addExtension(fetchRequest);
            return authRequest;
        } catch (MessageException | ConsumerException e) {
            Logging.getInstance().log(logger, "Exception occurred while building AuthRequest object: ", e);
            return null;
        }
    }

    /**
     * Processes the returned information from an authentication request
     * from the OP.
     *
     * @param discoveryInformation DiscoveryInformation that was created earlier
     *  in the conversation (by openid4java). This will need to be verified with
     *  openid4java to make sure everything went smoothly and there are no
     *  possible problems. This object was probably stored in session and retrieved
     *  for use in calling this method.
     *
     * @param pageParameters PageParameters passed to the page handling the
     *  return verification.
     *
     * @param returnToUrl The "return to" URL that was passed to the OP. It must
     *  match exactly, or openid4java will issue a verification failed message
     *  in the logs.
     *
     * @return AuthData - null if there was a problem, or a AuthData
     *  object, with parameters filled in as compeletely as possible from the
     *  information available from the OP. If you are using MyOpenID, most of the
     *  time what is returned is from your "Default" profile, so if you need more
     *  information returned, make sure your Default profile is completely filled
     *  out.
     */
    public static AuthData processReturn(
            DiscoveryInformation discoveryInformation, Map<String, String> pageParameters, String returnToUrl) {
        // Verify the Information returned from the OP
        // This is required according to the spec
        final ParameterList response = new ParameterList(pageParameters);
        try {
            final AuthData authData = new AuthData();
            final VerificationResult verificationResult =
                    getConsumerManager().verify(returnToUrl, response, discoveryInformation);
            final Identifier verifiedIdentifier = verificationResult.getVerifiedId();
            if (verifiedIdentifier != null) {
                final AuthSuccess authSuccess = (AuthSuccess) verificationResult.getAuthResponse();
                if (authSuccess.hasExtension(AxMessage.OPENID_NS_AX)) {
                    final MessageExtension extension = authSuccess.getExtension(AxMessage.OPENID_NS_AX);
                    if (extension instanceof FetchResponse) {
                        authData.setOpenId(verifiedIdentifier.getIdentifier());
                        final FetchResponse axResponse = (FetchResponse) extension;
                        final String emailAddress = axResponse.getAttributeValue("email");
                        if (emailAddress != null) {
                            authData.setEmailAddress(emailAddress);
                        }
                        final String firstName = axResponse.getAttributeValue("firstName");
                        if (firstName != null) {
                            authData.setFirstName(firstName);
                        }
                        final String lastName = axResponse.getAttributeValue("lastName");
                        if (lastName != null) {
                            authData.setLastName(lastName);
                        }
                        final String fullName = axResponse.getAttributeValue("fullname");
                        if (fullName != null) {
                            final int spaceIndex = fullName.lastIndexOf(' ');
                            if (spaceIndex == -1) {
                                if (authData.getFirstName() == null) {
                                    authData.setFirstName(fullName);
                                }
                            } else {
                                if (authData.getFirstName() == null) {
                                    authData.setFirstName(fullName.substring(0, spaceIndex));
                                }
                                if (authData.getLastName() == null) {
                                    authData.setLastName(fullName.substring(spaceIndex + 1));
                                }
                            }
                        }
                    }
                } else {
                    logger.warning("Server did not return OPENID_NS_AX!");
                }
                return authData;
            } else {
                return null;
            }
        } catch (MessageException | AssociationException | DiscoveryException e) {
            Logging.getInstance().log(logger, "Exception occurred while verifying response: ", e);
            return null;
        }
    }

    /**
     * Generates the returnToUrl parameter that is passed to the OP. The User Agent (i.e., the browser) will be directed
     * to this page following authentication.
     */
    public static String getReturnToUrl() {
        final FacesContext facesContext = FacesContext.getCurrentInstance();
        final ServletRequest request = (ServletRequest) facesContext.getExternalContext().getRequest();
        return "http://" + request.getServerName() + ":" + request.getServerPort()
                + facesContext.getApplication().getViewHandler().getActionURL(facesContext, "/NewAccount.xhtml");
    }

    private static ConsumerManager getConsumerManager() {
        if (consumerManager == null) {
            consumerManager = new ConsumerManager();
            consumerManager.setAssociations(new InMemoryConsumerAssociationStore());
            consumerManager.setNonceVerifier(new InMemoryNonceVerifier(10000));
        }
        return consumerManager;
    }
}
