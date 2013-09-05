package org.ffplanner;

import org.ffplanner.controller.AuthController;
import org.ffplanner.entity.User;
import org.ffplanner.util.JsfViews;

import javax.inject.Inject;
import javax.servlet.*;
import javax.servlet.annotation.WebFilter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;

/**
 * @author Bogdan Dumitriu
 */
@WebFilter(filterName = "loginFilter")
public class LoginFilter implements Filter {

    @Inject
    private AuthController authController;

    @Override
    public void init(FilterConfig filterConfig) throws ServletException {
    }

    @Override
    public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain)
            throws IOException, ServletException {
        final HttpServletRequest request = (HttpServletRequest) servletRequest;
        final HttpServletResponse response = (HttpServletResponse) servletResponse;
        final HttpSession session = request.getSession();

        final User user = authController.getUser();
        if (user == null) {
            final String queryString = request.getQueryString();
            final StringBuffer requestURL = request.getRequestURL();
            if (requestURL != null) {
                session.setAttribute("redirectTo", requestURL + (queryString == null ? "" : "?" + queryString));
            }
            response.sendRedirect(request.getContextPath() + "/faces" + JsfViews.LOGIN_TARGET.toString());
        } else {
            filterChain.doFilter(servletRequest, servletResponse);
        }
    }

    @Override
    public void destroy() {
    }
}
