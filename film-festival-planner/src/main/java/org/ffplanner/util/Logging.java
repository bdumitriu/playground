/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.util;

import java.util.logging.Logger;

/**
 * @author Bogdan Dumitriu
 */
public class Logging {

    private static final Logging instance = new Logging();

    public static Logging getInstance() {
        return instance;
    }

    private Logging() {
    }

    public void log(Logger logger, String messagePrefix, Exception e) {
        final String message = messagePrefix + e.getMessage();
        logger.severe(message);
        throw new RuntimeException(message, e);
    }
}
