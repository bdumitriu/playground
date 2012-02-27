/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.controller.auth;

import org.openid4java.association.AssociationException;
import org.openid4java.consumer.*;
import org.openid4java.discovery.DiscoveryException;
import org.openid4java.discovery.DiscoveryInformation;
import org.openid4java.discovery.Identifier;
import org.openid4java.message.*;
import org.openid4java.message.ax.AxMessage;
import org.openid4java.message.ax.FetchRequest;
import org.openid4java.message.ax.FetchResponse;

import java.net.URL;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

/**
 * @author Bogdan Dumitriu
 */
public class RegistrationService {

    private static final String YAHOO_ENDPOINT = "https://me.yahoo.com";

    private static final String GOOGLE_ENDPOINT = "https://www.google.com/accounts/o8/id";

    private static final Logger log = Logger.getLogger(RegistrationService.class.getName());

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
            final String message = "Error occurred during discovery: " + e.getMessage();
            log.severe(message);
            throw new RuntimeException(message, e);
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
            final URL opEndpoint = discoveryInformation.getOPEndpoint();
            fetchRequest.addAttribute("email", "http://axschema.org/contact/email", true);
            fetchRequest.addAttribute("firstName", "http://axschema.org/namePerson/first", true);
            fetchRequest.addAttribute("lastName", "http://axschema.org/namePerson/last", true);
            fetchRequest.addAttribute("fullname", "http://axschema.org/namePerson", true);
            authRequest.addExtension(fetchRequest);
            return authRequest;
        } catch (MessageException | ConsumerException e) {
            final String message = "Exception occurred while building AuthRequest object: " + e.getMessage();
            log.severe(message);
            throw new RuntimeException(message, e);
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
     * @return RegistrationModel - null if there was a problem, or a RegistrationModel
     *  object, with parameters filled in as compeletely as possible from the
     *  information available from the OP. If you are using MyOpenID, most of the
     *  time what is returned is from your "Default" profile, so if you need more
     *  information returned, make sure your Default profile is completely filled
     *  out.
     */
    public static RegistrationModel processReturn(
            DiscoveryInformation discoveryInformation, Map<String, String> pageParameters, String returnToUrl) {
        // Verify the Information returned from the OP
        // This is required according to the spec
        final ParameterList response = new ParameterList(pageParameters);
        try {
            final RegistrationModel registrationModel = new RegistrationModel();
            final VerificationResult verificationResult =
                    getConsumerManager().verify(returnToUrl, response, discoveryInformation);
            final Identifier verifiedIdentifier = verificationResult.getVerifiedId();
            if (verifiedIdentifier != null) {
                final AuthSuccess authSuccess = (AuthSuccess) verificationResult.getAuthResponse();
                if (authSuccess.hasExtension(AxMessage.OPENID_NS_AX)) {
                    final MessageExtension extension = authSuccess.getExtension(AxMessage.OPENID_NS_AX);
                    if (extension instanceof FetchResponse) {
                        registrationModel.setOpenId(verifiedIdentifier.getIdentifier());
                        final FetchResponse axResponse = (FetchResponse) extension;
                        final String emailAddress = axResponse.getAttributeValue("email");
                        if (emailAddress != null) {
                            registrationModel.setEmailAddress(emailAddress);
                        }
                        final String firstName = axResponse.getAttributeValue("firstName");
                        if (firstName != null) {
                            registrationModel.setFirstName(firstName);
                        }
                        final String lastName = axResponse.getAttributeValue("lastName");
                        if (lastName != null) {
                            registrationModel.setLastName(lastName);
                        }
                        final String fullName = axResponse.getAttributeValue("fullname");
                        if (fullName != null) {
                            final int spaceIndex = fullName.lastIndexOf(' ');
                            if (spaceIndex == -1) {
                                if (registrationModel.getFirstName() == null) {
                                    registrationModel.setFirstName(fullName);
                                }
                            } else {
                                if (registrationModel.getFirstName() == null) {
                                    registrationModel.setFirstName(fullName.substring(0, spaceIndex));
                                }
                                if (registrationModel.getLastName() == null) {
                                    registrationModel.setLastName(fullName.substring(spaceIndex + 1));
                                }
                            } 
                        }
                    }
                } else {
                    log.warning("Server did not return OPENID_NS_AX!");
                }
            }
            return registrationModel;
        } catch (MessageException | AssociationException | DiscoveryException e) {
            final String message = "Exception occurred while verifying response: " + e.getMessage();
            log.severe(message);
            throw new RuntimeException(message, e);
        }
    }

    /**
     * Generates the returnToUrl parameter that is passed to the OP. The User Agent (i.e., the browser) will be directed
     * to this page following authentication.
     */
    public static String getReturnToUrl() {
        return "http://localhost:8080/film-festival-planner-1.0-SNAPSHOT/faces/OpenID.xhtml";
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
