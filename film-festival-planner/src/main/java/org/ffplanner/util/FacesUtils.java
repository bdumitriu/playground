package org.ffplanner.util;

import javax.faces.application.ConfigurableNavigationHandler;
import javax.faces.application.NavigationCase;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import java.net.MalformedURLException;
import java.net.URL;

/**
 * @author Bogdan Dumitriu
 */
public class FacesUtils {

    public static String getUrlToRoot(FacesContext facesContext) {
        final ExternalContext externalContext = facesContext.getExternalContext();
        return externalContext.getRequestContextPath();
    }

    public static String getUrlToView(FacesContext facesContext, String viewId) throws MalformedURLException {
        final ConfigurableNavigationHandler navigationHandler =
                (ConfigurableNavigationHandler) facesContext.getApplication().getNavigationHandler();
        final NavigationCase navigationCase = navigationHandler.getNavigationCase(facesContext, null, viewId);
        final URL url = navigationCase.getBookmarkableURL(facesContext);
        return url.toExternalForm();
    }
}
