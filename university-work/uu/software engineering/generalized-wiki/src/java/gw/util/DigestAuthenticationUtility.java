package gw.util;


import java.security.*;
import java.util.Random;
import java.io.*;


/**
 * Provides some digest authentication utility functions.
 * It is a partial implementation of RFC 2617.
 */
public final class DigestAuthenticationUtility {
    private static final String CHARSET  = "ISO-8859-1";
    private static final String DIGESTOR = "MD5";
    
    // Left out the 1 and the 0 because they look to much like the letter L and the capital
    // O (oh)

    private static final char[] ALPHABET = { 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'
    										,'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r'
    										,'s', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A'
    										,'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'
    										,'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S'
    										,'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '2', '3'
    										,'4', '5', '6', '7', '8', '9'
    										}; 
    										
    public  static final String	REALM	 = "Generalized Wiki";


    /**
     * Returns the resulting hash which should be supplied by the client. This is the
     * default case with no qop-header set.
     * @param passwordHash the hash of the password of the client as returned by the
     *                     getPasswordHash method.
     * @param nonce The 'nonce' value.
     * @param method The HTTP Request Method (e.g. GET or POST).
     * @param uri The URI as specified by the client.
     * @return The resulting hash of concatenation of these values. The client should have
     *         provided exactly the same hash.
     */
    public static String getResultHash(String passwordHash, String nonce, String method, String uri) {
        return getHash(passwordHash + ":" + nonce + ":" + getUriHash(method, uri));
    }


    /**
     * Returns the resulting hash which should be supplied by the client. This is the case
     * for which the qop-header is set to "auth".
     * @param passwordHash the hash of the password of the client as returned by the
     *                     getPasswordHash method.
     * @param nonce nonce The 'nonce' value.
     * @param ncValue The 'ncValue'.
     * @param cnonce The 'cnonce' value.
     * @param qop The 'qop' value
     * @param method The HTTP Request Method (e.g. GET or POST).
     * @param uri The URI as specified by the client.
     * @return The resulting hash of concatenation of these values. The client should have
     *         provided exactly the same hash.
     */
    public static String getResultHash(String passwordHash, String nonce, String ncValue, String cnonce, String qop, String method, String uri) {
        return getHash(passwordHash + ":" + nonce + ":" + ncValue + ":" + cnonce + ":" + qop + ":" + getUriHash(method, uri));
    }


    /**
     * Returns the hash of the username and password. It also takes the realm into account,
     * so the realm should be constant if this hash is precomputed.
     * @param username The username.
     * @param password The password.
     * @return The password hash.
     */
    public static String getPasswordHash(String username, String password) {
        return getHash(username + ":" + REALM + ":" + password);
    }
    
    
    /**
     * Returns the hash used for MD5-Sess authentication. It uses the first nonce and cnonse
     * as supplied by both server and client.
     * @param nonce    The nonce.
     * @param cnonce   The cnonce.
     * @return The session password hash.
     */
    public static String getSessPasswordHash(String passwordHash, String nonce, String cnonce) {
        return getHash(passwordHash + ":" + nonce + ":" + cnonce);
    }


    /**
     * Returns the hash of the URI.
     * @param method The HTTP Method (e.g. GET or POST) name (in uppercase).
     * @param uri The URI.
     * @return The hash of the URI.
     */
    public static String getUriHash(String method, String uri) {
        return getHash(method + ":" + uri);
    }


    /**
     * Computes a hash of the input string, with the result encoded using the HEX
     * format specified in RFC 2617.
     * @param input The input string.
     * @return The output string.
     */
    private static String getHash(String input) {
        try {
            final MessageDigest digestor = MessageDigest.getInstance(DIGESTOR);

            final byte[] hashInput  = input.getBytes(CHARSET);
            final byte[] hashOutput = digestor.digest(hashInput);

            return toHexString(hashOutput);
        }
        catch(NoSuchAlgorithmException e) {
            throw new RuntimeException("No MD5 algorithm available: " + e.toString());
        }
        catch(UnsupportedEncodingException e) {
            throw new RuntimeException("No " + CHARSET + " encoding available: " + e.toString());
        }
    }


    /**
     * Converts the input hash to the HEX format as described in RFC 2617.
     * @param source Input bytes resulting from the hash function.
     * @return the HEX string.
     */
    public static String toHexString(byte[] source) {

        final StringBuffer buffer = new StringBuffer();

        for( int i = 0; i < source.length; ++i )
            buffer.append( Integer.toHexString(0x0100 + (source[i] & 0x00FF)).substring(1));

        return buffer.toString();
    }

    /**
     * Converts the input hash (as string) to the HEX format as described in RFC 2617.
     */
    public static String toHexString( final String source ) {

        try {

            return toHexString( source.getBytes( CHARSET ) );
        } catch( UnsupportedEncodingException e )  {

          throw new RuntimeException("No " + CHARSET + " encoding available: " + e.toString());  
        }
    }
    
    /**
     * Returns a random password in plain text. It uses both the
     * username and the time for the seed.
     * @param username The username.
     * @return A random password.
     */
    public static String getRandomPassword( String username ) {
    	
    	int givenSeed		= 0;
    	int maxPassLength	= 10;

    	byte[] givenSeedArr	= username.getBytes();
    	
    	for(int i = 0; i < givenSeedArr.length; ++i )
    		givenSeed += givenSeedArr[i];
    		
    	Random givenRandom 	= new Random( givenSeed );
    	Random timedRandom	= new Random( );
    	
    	Random combinedRandom	= new Random( givenRandom.nextInt() + timedRandom.nextInt() );
    	
    	StringBuffer genPasswd = new StringBuffer();
    	
    	for( int i = 0; i < maxPassLength; ++i ) {
    		
    		int alphabetIndex	= combinedRandom.nextInt(Integer.MAX_VALUE)%ALPHABET.length;
			genPasswd.append( ALPHABET[alphabetIndex] );	
    	}
    	
		return genPasswd.toString();
    }
}
