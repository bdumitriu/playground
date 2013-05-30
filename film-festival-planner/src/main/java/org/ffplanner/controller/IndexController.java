package org.ffplanner.controller;

import org.ffplanner.util.FacesUtils;
import org.ffplanner.util.Logging;

import javax.enterprise.context.RequestScoped;
import javax.faces.context.FacesContext;
import javax.inject.Named;
import java.net.MalformedURLException;
import java.util.logging.Logger;

/**
 * @author Bogdan Dumitriu
 */
@RequestScoped
@Named
public class IndexController {

    private static final Logger logger = Logger.getLogger(IndexController.class.getName());

    public String getRedirectUrl() {
        try {
            return FacesUtils.getUrlToView(FacesContext.getCurrentInstance(), "/auth/Program.xhtml");
        } catch (MalformedURLException e) {
            Logging.getInstance().log(logger, "Malformed root URL: ", e);
            return "http://www.google.com/";
        }
    }
}
