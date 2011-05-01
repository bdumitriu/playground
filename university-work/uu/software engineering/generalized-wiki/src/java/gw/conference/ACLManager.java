package gw.conference;

import gw.storage.Storage;
import gw.storage.StorageException;
import gw.users.User;
import gw.users.acl.ACLPermission;
import gw.users.acl.GwACLPermissions;
import gw.users.acl.StorageACLAdapter;
import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.Map.Entry;

public class ACLManager {

	private Storage _storage;
	private Conference _conference;
    
	public ACLManager(Conference conference) {
		_conference = conference;
        _storage = conference.getStorage();
	}

    /**
     * Sets the permissions for an author for his own author-page on the wiki
     * 
     * @param user The user him- or herself
     * @param path The path to his or her author page
     */
    public void setAuthorPagePermissions(User user, String path) throws StorageException{
        ArrayList<ACLPermission> permissions = getAllRights();        
        
        // Set the permissions for the author himself
        setPermissions(path, permissions, _conference.getChairperson());
        setPermissions(path, permissions, user.getId());
        // TODO Get the name of the 'everyone' group
        setPermissions(path, getReadRights(), "everyone");        
	}
    
    public void setPaperRights(Paper paper) throws StorageException {
        ConferenceState state = _conference.getState();
        Map<String,ArrayList<ACLPermission>> paperRights = new HashMap<String,ArrayList<ACLPermission>>();
        Map<String,ArrayList<ACLPermission>> reviewRights = new HashMap<String,ArrayList<ACLPermission>>();
        Map<String,ArrayList<ACLPermission>> commentRights = new HashMap<String,ArrayList<ACLPermission>>();        

        ArrayList<ACLPermission> allAccess = getAllRights();
        ArrayList<ACLPermission> readAccess = getReadRights();
        
        //the chairperson always has all the accessrights
        paperRights.put(_conference.getChairperson(), getAllRightsPlus());

        //authors are always allowed to read their own paper        
        setAuthorRights(paper, state, paperRights, allAccess, readAccess);
        
        setReviewerRights(paper, state, paperRights, allAccess, readAccess);
        
        setPCMembergRights(state, paperRights, reviewRights, commentRights, readAccess);
        
        //TODO Get the name of the 'everyone' group
        if(state.everyoneCanReadPapers())
            paperRights.put("everyone", readAccess);
        
        // Write these permissions       
        
        setPermissions(paper.getPath(), paperRights);
        
        for(String reviewer : paper.getReviewers()) {
            String path = paper.getReviewPath(reviewer);
            setPermissions(path, reviewRights);
        }

        setPermissions(paper.getDiscussionPath(), commentRights);
    }

    private void setPCMembergRights(ConferenceState state, Map<String, ArrayList<ACLPermission>> paperRights, Map<String, ArrayList<ACLPermission>> reviewRights, Map<String, ArrayList<ACLPermission>> commentRights, ArrayList<ACLPermission> readAccess) {
        for(String pcMember : _conference.getPCMembers()) {
            if(state.pcMembersCanReadPapers())
                paperRights.put(pcMember, readAccess);
            
            if(state.pcMembersCanAddComments())
                commentRights.put(pcMember, readAccess);
            
            if(state.pcMembersCanReadReviews())
                reviewRights.put(pcMember, readAccess);
        }
    }

    private void setReviewerRights(Paper paper, ConferenceState state, Map<String, ArrayList<ACLPermission>> paperRights, ArrayList<ACLPermission> allAccess, ArrayList<ACLPermission> readAccess) throws StorageException {
        for(String reviewer : paper.getReviewers()) {
            if(state.reviewersCanReadPapers())
                paperRights.put(reviewer, readAccess);

            if(state.reviewersCanAddReviews())
                // Rights for review page (e.g. /conference/Papers/BlaPaper/Reviews/ReviewA)
                setPermissions(paper.getReviewPath(reviewer), allAccess, reviewer);
            
        }
    }

    private void setAuthorRights(Paper paper, ConferenceState state, Map<String, ArrayList<ACLPermission>> paperRights, ArrayList<ACLPermission> allAccess, ArrayList<ACLPermission> readAccess) {
        for(String author : paper.getAuthors()) {
            if(state.authorsCanEditPaper()) 
                paperRights.put(author, allAccess);
            else 
                paperRights.put(author, readAccess);
        }
    }
   
    
	/**
	 * Sets a certain permission for a number of users on a certain wiki page
	 * 
	 * @param path The path to the page of the wiki
	 */
    private void setPermissions(String path, ArrayList<ACLPermission> permissions, String user) throws StorageException {
        StorageACLAdapter sad = new StorageACLAdapter(path, _storage);
        for(int t = 0; t < permissions.size(); t++) {
            sad.addACLRight(permissions.get(t), user);
        }
    }
    
    private void setPermissions(String path, Map<String, ArrayList<ACLPermission>> rights) throws StorageException {
        StorageACLAdapter sad = new StorageACLAdapter(path, _storage);
        
        for(Entry<String, ArrayList<ACLPermission>> entry : rights.entrySet()) {
            String userId = entry.getKey();
            ArrayList<ACLPermission> permissions = entry.getValue();
            for(ACLPermission p : permissions) {
                sad.addACLRight(p, userId);
            }
        }
    }

    private static ArrayList<ACLPermission> getReadRights() {
        ArrayList<ACLPermission> readAccess = new ArrayList<ACLPermission>();
        readAccess.add(GwACLPermissions.READ_PERMISSION);
        return readAccess;
    }

    private static ArrayList<ACLPermission> getAllRights() {
        ArrayList<ACLPermission> allAccess = new ArrayList<ACLPermission>();
        allAccess.add(GwACLPermissions.READ_PERMISSION);
        allAccess.add(GwACLPermissions.WRITE_PERMISSION);
        allAccess.add(GwACLPermissions.DELETE_PERMISSION);
        return allAccess;
    }    

    private static ArrayList<ACLPermission> getAllRightsPlus() {
        ArrayList<ACLPermission> allAccess = getAllRights();
        allAccess.add(GwACLPermissions.ACL_WRITE_PERMISSION);
        return allAccess;
    }

}