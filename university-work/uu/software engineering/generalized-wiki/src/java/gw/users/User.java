package gw.users;

import java.util.*;

/**
 * Storage facility of user properties. It represents the user and
 * its properties. The idea is that you can initialize a user and
 * pass it anywhere and sync the contents later.
 */

public class User {
    private static final String ANONYMOUS_USER_ID = "AnonyMous";
    public static final User ANONYMOUS_USER = new User( ANONYMOUS_USER_ID );
    
	private String _id;
	private Map<String, String> _properties;
	
	/**
	 * Constructs a new User object.
	 * @param id The username
	 * @param email The user's email address
	 */
	protected User( final String id, final String email) {
		
		setProperties( new HashMap<String, String>() );
        setId( id );
		setEmail( email );
	}
    
	/**
	 * Construcs a new User object
	 * @param id The username
	 */
    public User(final String id) {
    	
		setProperties( new HashMap<String, String>() );
		setId( id );
    }
	
	/**
	 * Accessor to set the username
	 * @param id The username
	 */
	private void setId( final String id ) {
			
		_id = id;
	}
	
	/**
	 * Accessor to set the email address
	 * @param email The user's email address
	 */
	public void setEmail( final String email ) {
        getProperties().put("Email", email);
	}
    
	/**
	 * Accessor to set additional properties
	 * @param map Map containing all the user's properties
	 */
    private void setProperties( final Map<String, String> map ) {
    	
    	if( map == null )
			throw new IllegalArgumentException( "gw.users.User.map cannot be null");
			
		_properties = map;
    }
    
	/**
	 * Accessor to set an arbitrary property to the user
	 * @param key Left-value of the property
	 * @param value Right-value of the property
	 */
    public void setProperty( final String key, final String value ) {
    	
    	if( key == null )
			throw new IllegalArgumentException( "gw.users.User.key cannot be null");
		
        getProperties().put( key, value );
    }
    
	/**
	 * Accessor to get the username
	 * @return The username
	 */
    public String getId( ) {
    	
        return _id;
    }
    
	/**
	 * Accessor to get the email address
	 * @return The user's email address
	 */
    public String getEmail( ){
    	
        return getProperties().get("Email").toString();
    }
    
	/**
	 * Accessor to get the user's properties
	 * @return A map containing all the user's properties
	 */
    public Map<String, String> getProperties() {
    	
        return _properties;
    }
    
	/**
	 * Accessor to get a specific property of the user
	 * @param key The wanted property's left-value
	 * @return The wanted property's right-value
	 */
    public String getProperty( final String key ) {
    	
    	return (String) getProperties().get( key );
    }
}
