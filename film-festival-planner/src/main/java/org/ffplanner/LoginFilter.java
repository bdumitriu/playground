/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner;

import org.ffplanner.controller.AuthController;

import javax.servlet.*;
import javax.servlet.annotation.WebFilter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;

/**
 * @author Bogdan Dumitriu
 */
@WebFilter("/faces/auth/*")
public class LoginFilter implements Filter {

    @Override
    public void init(FilterConfig filterConfig) throws ServletException {
    }

    @Override
    public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain)
            throws IOException, ServletException {
        final HttpServletRequest request = (HttpServletRequest) servletRequest;
        final HttpServletResponse response = (HttpServletResponse) servletResponse;
        final HttpSession session = request.getSession();
        final AuthController auth = (AuthController) session.getAttribute("auth");

        if (auth == null || auth.getUser() == null) {
            final String queryString = request.getQueryString();
            final StringBuffer requestURL = request.getRequestURL();
            if (requestURL != null) {
                session.setAttribute("redirectTo", requestURL + (queryString == null ? "" : "?" + queryString));
            }
            response.sendRedirect(request.getContextPath() + "/faces/Login.xhtml");
        } else {
            filterChain.doFilter(servletRequest, servletResponse);
        }
    }

    @Override
    public void destroy() {
    }
}
