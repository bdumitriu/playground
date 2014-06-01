package ro.bdumitriu.wisdomator.business.security.boundary;

import javax.servlet.*;
import javax.servlet.annotation.WebFilter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * @author Bogdan Dumitriu
 */
@WebFilter("/*")
public class Authenticator implements Filter {

    @Override
    public void init(FilterConfig filterConfig) throws ServletException {
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
        HttpServletRequest httpServletRequest = (HttpServletRequest) request;
        HttpServletResponse httpServletResponse = (HttpServletResponse) response;
        if (httpServletRequest.authenticate(httpServletResponse)) {
            chain.doFilter(request, response);
        }
    }

    @Override
    public void destroy() {
    }
}
