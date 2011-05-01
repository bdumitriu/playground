package gw.users;

import java.util.*;
import java.io.*;

import gw.storage.*;
import gw.users.acl.*;
import gw.util.DigestAuthenticationUtility;

/**
 * Implementation of <tt>UserDataAdapter</tt> to save <tt>User</tt> objects on disk.
 * This objects are stored in a directory "Users" in a given path.
 * 
 */
public class StorageUserDataAdapter implements UserDataAdapter {
    private static final String PASSWORD_PROPERTY_KEY  = "password";

	// The path to to directory where users will be saved
	private String _usersDir;
	
	// The persistent storage where we save all data of the users
	private Storage _store;

	/**
	 * Create a <tt>StorageUserDataAdapter</tt> that saves the <tt>User</tt> objects in a directory
	 * in the given rootPath.
	 * @param path Path were the Users directory must be created or already is.
	 */   
    public StorageUserDataAdapter(String path, Storage storage) throws StorageException {
        _store = storage;
        setUsersDir(path);
    }
	
	/**
	 * Set the directory where the users are saved. If this directory does not exist, it tries to create it.
	 * @param path Path to the usersdirectory.
	 */
	private void setUsersDir( String path ) {
		if (path == null)
			throw new IllegalArgumentException("gw.users.StorageUserDataAdapter.path: may not be null.");		
		try {
			if (!_store.isDirectory(path)) {
				_store.makeDirectory(path);
			}
		}
		catch (StorageException sEx) {
			throw new IllegalStateException("gw.users.StorageUserDataAdapter.usersDir: Can't create the " + path + " directory: " + sEx.toString());
		}
		if ('/' == path.charAt(path.length() - 1))
			_usersDir = path;
		else
			_usersDir = path + "/";
	}
	
	/**
	 * Get the path to the usersdirectory
	 * @return the path to the usersdirectory
	 */
	private String getUsersDir( ) {
		return _usersDir;
	}
	
    
    /**
     * Get the full path relative to the root of the repository for the user.
     */
	private String getUserDir( final User user) {
		
		return _usersDir + user.getId() + "/";
	}


    /**
     * Creates the user.
     */
	public void create(final User user, final String password ) {
	
		if( exists( user) )
			throw new UserExistsException( "Cannot create user " + user.getId() + ", user exists");	
		
        String userPath = _usersDir + user.getId();
        
		try { _store.makeDirectory(userPath); }
		catch (StorageException sEx) {
  			throw new IllegalStateException( "Unable to create directory: " + _usersDir);
		}
		store( user );
        setPassword( user, password );
        StorageUserAliasDictionary dict = new StorageUserAliasDictionary( _store );
        
        try { 
        	
        	setPermissionsOfUser(userPath, user.getId()); 
			dict.addAliasToAlias( StorageUserAliasDictionary.DEFAULT_GROUP, user.getId() );
		} catch(StorageException sEx) {
            throw new IllegalStateException("Unsable to set user permissions: " + _usersDir);
        }
	}
    
    
    /**
     * Sets the default permissions of the user to only allow access for this user.
     */
    private void setPermissionsOfUser(String path, String username) throws StorageException {
        ACLAdapter def = (new StorageACLAdapterFactory(_store)).getACLDefinition(new FilePathACLResource(path));
        
        for(int i=0; i < GwACLPermissions.ALL_PERMISSIONS.length; i++)
            def.setPermissionLine(GwACLPermissions.ALL_PERMISSIONS[i], username);
    }
    

    /**
     * Sets the password.
     */
    public void setPassword( User user, String password ) {

 		final String passDigest	= DigestAuthenticationUtility.getPasswordHash( user.getId()
 																			 , password );
        try {
            _store.setProperty( getUserDir( user ), PASSWORD_PROPERTY_KEY,
                                new String( passDigest ), false);
        } catch( StorageException storeEx ) {

            throw new IllegalStateException( "Could not set the password for user " + user.getId() );
        }
    }


    /**
     * Gets the password.
     */
    public String getPassword( User user ) {

        final Map userDirProperties;
        try {
        	
            userDirProperties   = _store.getProperties( getUserDir( user ) );
        } catch( StorageException storeEx ) {

            throw new IllegalStateException( "Could not get the password for user " + user.getId() );
        }

        return (String)userDirProperties.get( PASSWORD_PROPERTY_KEY );
    }


    /**
     * Deletes the user.
     */
    public void delete( final User user ) {
	
		if( !exists( user) )
			throw new NoSuchUserException( "User " + user.getId() + ", does not exists on this location" );	
			
		final String userDir	= getUserDir( user );

		try {
			_store.deleteFile(userDir, true);
		}
		catch (StorageException sEx) {
			throw new IllegalStateException( "Unable to delete directory " + userDir);
		}
	}

	/**
	 * Load the properties of the given user from the usersdirectory. The userid must be a existing userid to
	 * let this work. 
	 * @param user the user to be loaded.
	 */
	public void load(User user) {
		try {
		
			if (user == null)
				throw new IllegalArgumentException("gw.users.StorageUserDataAdapter.user: may not be null.");
        
			if( !exists( user) )
				throw new NoSuchUserException( "User "+user.getId() +", does not exist on this location" );	
        
			final String userDir	= getUserDir( user );
			final Iterator iter		= _store.getDirListing(userDir);
		
			while (iter.hasNext()) {

				final String key = (String) iter.next();
				final BufferedReader propertyReader = new BufferedReader(new InputStreamReader(_store.getFile(key)));
				final String value = propertyReader.readLine();

				user.getProperties().put(key.substring(key.lastIndexOf('/') + 1), value);
			}
		}
			
		catch (StorageException sEx) {
			throw new IllegalStateException("Unable to load user " + user.getId());
		}

		catch (IOException ioEx) {
			throw new IllegalStateException("Unable to load user " + user.getId());
		}
	}

	/**
	 * Store the properties of the given user to the usersdir.
	 * @param user the user to store.
	 */
	public void store(User user) {
		try {
	  
			if (user == null)
				throw new IllegalArgumentException("gw.users.StorageUserDataAdapter.user: may not be null.");

			if( !exists( user) ) {
				throw new NoSuchUserException( "User " + user.getId() + " does not exist (yet)" );
			}

			final String userDir	= getUserDir( user );
			final Iterator iter		= user.getProperties().keySet().iterator();
		
			while (iter.hasNext()) {
		
				final String property = (String) iter.next();
				final String value = (String) user.getProperties().get(property);
				final OutputStream writer = _store.storeFile(userDir + property);

				writer.write(value.getBytes());
				writer.flush();
				writer.close();
			}
		}

		catch (StorageException sEx) {
			throw new IllegalStateException("Unable to store user " + user.getId() + " :" + sEx.toString());
		}
		catch (IOException ioEx) {
			throw new IllegalStateException("Unable to store user " + user.getId() + " :" + ioEx.toString());
		}
	}

	/** 
	 * Get a list of all users that are stored on the location.
	 * @return a list with all users stored on the location.
	 */
	public List getAllUsers() {
		try {
		  
			final ArrayList users	= new ArrayList();
			final Iterator iter		= _store.getDirListing(getUsersDir());

			while (iter.hasNext()) {
			
				final String dir = (String) iter.next();
			
				if (_store.isDirectory(dir)) {
				
					// The Storage interface delivers the full path
					// so we need to strip the usersDir first.
					final String userId	= dir.substring(  getUsersDir().length()
														, dir.length() );
					final User user = new User(userId);
					load(user);
					users.add(user);
				}
			}
			return users;
		}

		catch (StorageException sEx) {
			throw new IllegalStateException("Unable to list all users in directory " + _usersDir);
		}
	}


    /**
     * Checks if a user exist.
     */
	public boolean exists( final User user ) {
		
		final String userDir = getUserDir( user );
		
		try {
			return _store.isDirectory(userDir);
			
		}
		catch (StorageException sEx) {
			throw new IllegalStateException("Unable to lookup user " + user.getId() 
											+ " on location " + userDir );
		}
	}
}
