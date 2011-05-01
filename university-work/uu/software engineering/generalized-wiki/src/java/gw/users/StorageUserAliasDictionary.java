package gw.users;

import java.util.*;
import java.io.*;
import gw.storage.*;
import gw.users.acl.*;


/**
 * Represents a dictionary that uses storage as back-end.
 * 
 * Groups are stored as files in the group directory. A group
 * file consist of zero or more lines, on each line the members
 * of a group. A member of a group can be a user or a group.
 * 
 * The group system, in essence, describes the aliasses for a
 * user. Every group a user is member of, is an alias of the user.
 * The sole purpose of this dictionary is to lookup the aliasses
 * of an user.
 */
public class StorageUserAliasDictionary implements UserAliasDictionary {
    private static final String GROUP_DIRECTORY = "/Groups";
    public  static final String DEFAULT_GROUP	= "DefaultGroup";
    public  static final String EMPTY_GROUP     = "NoBody";      
    
    private Storage _storage;
    
    
    /**
     * Constructs the dictionary using the group directory stored in
     * storage.
     * @param storage The storage.
     */
    public StorageUserAliasDictionary(Storage storage) {
        _storage = storage;
        
        try { setupGroupEnvironment(); }
        catch(StorageException exception) {
            throw new RuntimeException("Failed to setup the group environment, due to: " + exception.getMessage());
        }   
    }
    
    
    /**
     * Sets up the group environment if it does not exist yet.
     * @throws StorageException
     */
    private void setupGroupEnvironment() throws StorageException {
        if (!_storage.fileExists(GROUP_DIRECTORY))
            _storage.makeDirectory(GROUP_DIRECTORY);
        if (!_storage.isDirectory(GROUP_DIRECTORY))
            throw new RuntimeException("Group directory is not a directory.");
        
        if (!_storage.fileExists(GROUP_DIRECTORY + "/" + DEFAULT_GROUP))
            setAliassesOfAlias(DEFAULT_GROUP, new ArrayList());
        
        if (!_storage.fileExists(GROUP_DIRECTORY + "/" + EMPTY_GROUP))
            setAliassesOfAlias(EMPTY_GROUP, new ArrayList());
    }
    

    /** Returns the aliases of the given user.
     * @see gw.users.UserAliasDictionary#getAliases(java.lang.String)
     */
    public List getAliases(String userId) {
        GroupMembershipRules membershipRules = new GroupMembershipRules();
        membershipRules.loadFrom(GROUP_DIRECTORY, _storage);
        membershipRules.inverse();

        List initialAliasses = new ArrayList();
        initialAliasses.add(userId);

        List aliasses = membershipRules.apply(initialAliasses);
        return aliasses;
    }
    
    
    /**
     * @see gw.users.UserAliasDictionary#addAliasToAlias(java.lang.String, java.lang.String)
     */
    public void addAliasToAlias(String parentAlias, String childAlias) throws StorageException {
        List aliasses = getAliassesOfAlias(parentAlias);
        aliasses.remove(childAlias.trim());
        aliasses.add(childAlias.trim());
        setAliassesOfAlias(parentAlias, aliasses);
    }


    /**
     * @see gw.users.UserAliasDictionary#removeAliasFromAlias(java.lang.String, java.lang.String)
     */
    public void removeAliasFromAlias(String parentAlias, String childAlias) throws StorageException {
        List aliasses = getAliassesOfAlias(parentAlias);
        aliasses.remove(childAlias.trim());
        setAliassesOfAlias(parentAlias, aliasses);
    }
    
    
    /**
     * Gets the aliasses of the alias.
     * @param aliasName The alias.
     * @return The aliasses.
     * @throws StorageException
     */
    private List getAliassesOfAlias(String aliasName) throws StorageException {
        String parentFile     = GROUP_DIRECTORY + "/" + aliasName;
        
        if (!_storage.fileExists(parentFile))
            return new ArrayList();
        
        InputStream stream    = _storage.getFile(parentFile);
        BufferedReader reader = new BufferedReader(new InputStreamReader(stream));

        try {
            ArrayList childs = new ArrayList();
            String childLine = null;
            while((childLine = reader.readLine()) != null) {
                String childName = childLine.trim();
                if (!childName.equals(""))
                    childs.add(childLine);
            }
            
            reader.close();
            return childs;
        }        
        catch(IOException exception) {
            throw new StorageException("Unable to get aliasses of alias: " + parentFile + ", reason: " + exception.toString());
        }
    }    

    /**
     * Sets the aliasses of the alias.
     * @param aliasName The alias.
     * @param aliasses The aliasses.
     * @throws StorageException
     */    
    private void setAliassesOfAlias(String aliasName, List aliasses) throws StorageException {
        String parentFile     = GROUP_DIRECTORY + "/" + aliasName;
        OutputStream stream   = _storage.storeFile(parentFile);
        BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(stream));

        try {
            Iterator iterator = aliasses.iterator();
            while(iterator.hasNext()) {
                String alias = (String) iterator.next();
                writer.write(alias.trim());
                writer.newLine();
            }
            
            writer.flush();
            writer.close();
        }        
        catch(IOException exception) {
            throw new StorageException("Unable to set aliasses of alias: " + parentFile + ", reason: " + exception.toString());
        }
    }


    /**
     * Represents a set of membership rules which can be read in from the
     * group directory and applied to a an initial set of aliasses.
     */
    private static class GroupMembershipRules {
        private Map _rules;

        protected GroupMembershipRules() {
            _rules = new Hashtable();
        }
        
        /**
         * Loads the rules from a directory in storage.
         * @param groupDirectory The directory containing the groups.
         * @param storage The storage.
         */
        public void loadFrom(String groupDirectory, Storage storage) {
            try {
                Iterator groupsIterator = storage.getDirListing(groupDirectory);
                while(groupsIterator.hasNext()) {
                    String path = (String) groupsIterator.next();
                    FilePathACLResource resource = new FilePathACLResource(path);
                    
                    InputStream stream = storage.getFile(path);
                    addMembersFromGroup(resource.getName(), stream);
                }
            }
            catch(StorageException exception) {
                throw new RuntimeException("Could not load group rules from storage: " + exception.toString());
            }
        }


        /**
         * Adds user or group names from the groupfile, turning each line
         * into a rule.
         * @param stream the stream from which to load the groups.
         */
        private void addMembersFromGroup(String groupName, InputStream stream) {
            try {
                InputStreamReader streamReader = new InputStreamReader(stream);
                BufferedReader reader = new BufferedReader(streamReader);

                String line = null;
                while (null != (line = reader.readLine()))
                    if (!line.trim().equals(""))
                        addRule(_rules, groupName, line.trim());
                
                reader.close();
            } catch (IOException exception) {
                throw new RuntimeException(exception.toString());
            }
        }


        /**
         * For each mapping a -> b, generates the mapping b <- a.
         *
         * The following equality should hold:
         *   A.inverse().inverse() == A
         */
        public void inverse() {
            Map invertedRules = new HashMap();

            Iterator sourceIterator = _rules.keySet().iterator();
            while(sourceIterator.hasNext()) {
                String source = (String) sourceIterator.next();

                Iterator destIterator = ((List) _rules.get(source)).iterator();
                while(destIterator.hasNext()) {
                    String dest = (String) destIterator.next();
                    addRule(invertedRules, dest, source);
                }
            }

            _rules = invertedRules;
        }


        /**
         * Adds the given rule (source -> dest) to the
         * rules.
         * @param rules The rules.
         * @param source The source of the rule.
         * @param dest The destination of the rule.
         */
        private void addRule(Map rules, String source, String dest) {
            if (!rules.containsKey(source))
                rules.put(source, new ArrayList());

            ((List) rules.get(source)).add(dest);
        }


        /**
         * Computes the transitive closure of the rules to the
         * given input.
         * @param initialAliases The initial input.
         * @return The transitive closure.
         */
        public List apply(List initialAliases) {
            Set aliases = new HashSet(initialAliases);

            int previousSize = 0;
            while(aliases.size() != previousSize) {
                previousSize = aliases.size();

                Iterator aliasIterator = aliases.iterator();
                while(aliasIterator.hasNext()) {
                    String alias = (String) aliasIterator.next();

                    if (_rules.containsKey(alias)) {
                        List additionalAliasses = (List) _rules.get(alias);
                        aliases.addAll(additionalAliasses);
                    }
                }
            }

            return new ArrayList(aliases);
        }
    }
}
