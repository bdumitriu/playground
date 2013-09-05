package org.ffplanner;

import org.ffplanner.controller.AuthController;
import org.ffplanner.entity.User;
import org.ffplanner.entity.UserRole;
import org.ffplanner.util.JsfViews;

import javax.inject.Inject;
import javax.servlet.*;
import javax.servlet.annotation.WebFilter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.logging.Logger;

/**
 * @author Bogdan Dumitriu
 */
@WebFilter(filterName = "adminFilter")
public class AdminFilter implements Filter {

    private static final Logger logger = Logger.getLogger(AdminFilter.class.getName());

    @Inject
    private AuthController authController;

    @Override
    public void init(FilterConfig filterConfig) throws ServletException {
    }

    @Override
    public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain)
            throws IOException, ServletException {
        final User user = authController.getUser();
        if (user == null) {
            logger.severe("LoginFilter did not ensure a valid user.");
        } else if (user.hasRole(UserRole.ADMIN)) {
            filterChain.doFilter(servletRequest, servletResponse);
        } else {
            final HttpServletRequest request = (HttpServletRequest) servletRequest;
            final HttpServletResponse response = (HttpServletResponse) servletResponse;
            response.sendRedirect(request.getContextPath() + "/faces" + JsfViews.UNAUTHORISED_TARGET.toString());
        }
    }

    @Override
    public void destroy() {
    }
}
