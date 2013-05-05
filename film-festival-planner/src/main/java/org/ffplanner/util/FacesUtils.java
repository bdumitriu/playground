package org.ffplanner.util;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.servlet.ServletRequest;
import java.net.MalformedURLException;
import java.net.URL;

/**
 * @author Bogdan Dumitriu
 */
public class FacesUtils {

    public static String getUrlToRoot(FacesContext facesContext) throws MalformedURLException {
        final ExternalContext externalContext = facesContext.getExternalContext();
        final ServletRequest request = (ServletRequest) externalContext.getRequest();
        final URL url = new URL(request.getScheme(), request.getServerName(), request.getServerPort(),
                externalContext.getRequestContextPath());
        return url.toExternalForm();
    }

    public static String getUrlToView(FacesContext facesContext, String viewId) throws MalformedURLException {
        final ExternalContext externalContext = facesContext.getExternalContext();
        final ServletRequest request = (ServletRequest) externalContext.getRequest();
        final URL url = new URL(request.getScheme(), request.getServerName(), request.getServerPort(),
                facesContext.getApplication().getViewHandler().getActionURL(facesContext, viewId));
        return url.toExternalForm();
    }
}
