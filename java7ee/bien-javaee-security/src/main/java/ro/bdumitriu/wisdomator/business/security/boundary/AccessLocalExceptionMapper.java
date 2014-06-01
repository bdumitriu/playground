package ro.bdumitriu.wisdomator.business.security.boundary;

import javax.ejb.AccessLocalException;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.ext.ExceptionMapper;
import javax.ws.rs.ext.Provider;

/**
 * @author Bogdan Dumitriu
 */
@Provider
public class AccessLocalExceptionMapper implements ExceptionMapper<AccessLocalException> {

    @Context
    HttpServletRequest httpServletRequest;

    @Override
    public Response toResponse(AccessLocalException exception) {
        try {
            httpServletRequest.logout();
        } catch (ServletException ignored) {
        }
        return Response.status(Response.Status.UNAUTHORIZED).header("X-Message", exception.getMessage()).build();
    }
}
