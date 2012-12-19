package org.ffplanner.util;

import org.ffplanner.qualifier.Messages;

import javax.enterprise.inject.Produces;
import java.util.ResourceBundle;

/**
 * @author Bogdan Dumitriu
 */
public class ResourceBundleProducer {

    @Produces @Messages
    public ResourceBundle getResourceBundle() {
        return ResourceBundle.getBundle("/messages");
    }
}
