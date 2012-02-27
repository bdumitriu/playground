/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.controller.auth;

import java.io.Serializable;

/**
 * @author Bogdan Dumitriu
 */

public class RegistrationModel implements Serializable {

    private static final long serialVersionUID = -8667859852129851575L;

    private String openId;

    private String firstName;

    private String lastName;

    private String emailAddress;

    public String getOpenId() {
        return openId;
    }

    public void setOpenId(String openId) {
        this.openId = openId;
    }

    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String fullName) {
        this.firstName = fullName;
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    public String getEmailAddress() {
        return emailAddress;
    }

    public void setEmailAddress(String emailAddress) {
        this.emailAddress = emailAddress;
    }
}
