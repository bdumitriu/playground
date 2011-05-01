package gw.users.actions;

import gw.GwServlet;
import gw.GwSessionContext;
import gw.storage.SessionStorageFactory;
import gw.users.*;
import gw.util.DigestAuthenticationUtility;

import javax.servlet.ServletException;
import javax.servlet.http.*;
import java.io.IOException;
import java.util.*;

/**
 * @author Arie Middelkoop
 * @author John van Schie
 */
public class ChallengeResponseLoginServlet extends GwServlet {
    /**
     * Handles the authentication request. If no authentication information is provided, it sends a chaet llenge to the
     * client. The client responds with the user information. The digest authentication scheme is used as documented in
     * RFC 2617.
     *
     * @param request  The request.
     * @param response The response if logged in.
     * @throws ServletException Problem with the servlet.
     * @throws IOException      Problem with IO.
     */
    public void doGet( HttpServletRequest request, HttpServletResponse response ) throws ServletException, IOException {
        handleLoginRequest( request, response );
    }

    /**
     * Handles the authentication request. If no authentication information is provided, it sends a challenge to the
     * client. The client responds with the user information. The digest authentication scheme is used as documented in
     * RFC 2617.
     *
     * @param request  The request.
     * @param response The response if logged in.
     * @throws ServletException Problem with the servlet.
     * @throws IOException      Problem with IO.
     */
    public void doPost( HttpServletRequest request, HttpServletResponse response ) throws ServletException, IOException {
        handleLoginRequest( request, response );
    }

    /**
     * Handles the authentication request. If no authentication information is provided, it sends a challenge to the
     * client. The client responds with the user information. The digest authentication scheme is used as documented in
     * RFC 2617.
     *
     * @param request  The request.
     * @param response The response if logged in.
     * @throws ServletException Problem with the servlet.
     * @throws IOException      Problem with IO.
     */
    private void handleLoginRequest( HttpServletRequest request, HttpServletResponse response ) throws ServletException, IOException {
        // initialize the storage object
        GwSessionContext gsc = getSessionContext(request);

        boolean isAuthenticationRequired    = true;

        if( isAuthenticateRequest( request, response ) )
            if( login( request ) ) {
                String referer = (String) request.getSession().getAttribute( "referer" );

                if( referer == null || referer.equals( "" ) || referer.equals( request.getContextPath() + request.getServletPath() ) ) {
                    referer = "/view/";
                }
                request.getSession().setAttribute( "referer", "/view/" );
                isAuthenticationRequired = false;
                getServletConfig().getServletContext().getRequestDispatcher( referer ).forward( request, response );
            }

        if( isAuthenticationRequired )
            sendAuthenticationRequest( request, response );
    }


    /**
     * Checks if the authentication is valid and logs in the user.
     * Called if the request is an authentication request.
     *
     * @param request The request.
     * @return True if the authentication information is valid, false otherwise.
     */
    private boolean login( HttpServletRequest request ) throws ServletException {
        GwSessionContext gsc = getSessionContext(request);
        SessionAuthenticationManager sam = gsc.getSam();
        Map authParameters = getAuthorizationParameters( request );
  
        boolean error = false;
        boolean loginOk = false;
        String opaque = (String) authParameters.get( "opaque" );
        String userId = (String) authParameters.get( "username" );
        
        if( !opaque.equals( DigestAuthenticationUtility.toHexString( request.getSession().getId() ) ) )
            error = true;
        
        if(!error)
            loginOk = sam.login( userId, authParameters );

        sam.setNonce( null );
        return loginOk;
    }


    /**
     * Extracts the parameters out of the authorization header.
     *
     * @param authorizationHeaderValue The value of the authorization header.
     * @return The authorization parameters.
     */
    private Map getAuthorizationParameters( HttpServletRequest request ) {
        String authorizationHeaderValue = request.getHeader( "Authorization" );
        
        int digestIndex = authorizationHeaderValue.indexOf( "Digest" );
        if( digestIndex < 0 )
            throw new RuntimeException( "Invalid authorization type. Should be Digest." );

        Map parameters = new HashMap();
        String authorizationString = authorizationHeaderValue.substring( digestIndex + "Digest".length() + 1 );
        StringTokenizer tokenizer = new StringTokenizer( authorizationString, "," );
        while( tokenizer.hasMoreTokens() ) {
            String keyValuePair = tokenizer.nextToken();

            int equalIndex = keyValuePair.indexOf( "=" );
            if( equalIndex < 0 )
                throw new RuntimeException( "Invalid authorization header specified." );

            String key = keyValuePair.substring( 0, equalIndex ).trim();
            String value = keyValuePair.substring( equalIndex + 1 ).replace( '"', ' ' ).trim();

            parameters.put( key, value );
        }
        
        parameters.put( "requestMethod", request.getMethod() );

        return parameters;
    }


    /**
     * Puts an authentication request in the response to the client.
     * Stores the challenge in the SessionAuthenticationManager associated with the
     * session context.
     *
     * @param request  The request of the client.
     * @param response The response.
     * @throws IOException Thrown if an IO error occurs.
     */
    private void sendAuthenticationRequest( HttpServletRequest request, HttpServletResponse response ) throws IOException, ServletException {
        final Random random = new Random();
        final Date date = new Date();
        final String nonce = DigestAuthenticationUtility.toHexString( "" + random.nextInt() + date.getTime() );

        SessionAuthenticationManager sam = getSessionContext(request).getSam();
        
        sam.setNonce( nonce ); sam.setCNonce( null );

        final String authDetails = "Digest "
                + "realm=\"" + DigestAuthenticationUtility.REALM + "\", "
                + "nonce=\"" + nonce + "\", "
                + "qop=\"auth\", "
                + "opaque=\"" + DigestAuthenticationUtility.toHexString( request.getSession().getId() ) + "\", "
                + "algorithm=\"MD5\"";

        response.setHeader( "WWW-Authenticate", authDetails );
        response.sendError( HttpServletResponse.SC_UNAUTHORIZED );
    }


    /**
     * Checks if an Authorization header is present in the request. This indicates an authentication request.
     *
     * @param request  The request.
     * @param response The responce.
     * @return True if there is authentication information present, false otherwise.
     */
    private boolean isAuthenticateRequest( HttpServletRequest request, HttpServletResponse response ) {
        return request.getHeader( "Authorization" ) != null;
    }
}
