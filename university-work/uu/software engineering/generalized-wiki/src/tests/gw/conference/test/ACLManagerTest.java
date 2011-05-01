package gw.conference.test;

import java.util.HashMap;

import gw.conference.*;
import gw.storage.Storage;
import gw.users.User;
import junit.framework.TestCase;

public class ACLManagerTest extends TestCase {
	private ACLManager _acl;
	private Conference _conference;
    private Storage _storage;
    private User _user;
    
    public void setUp() throws Exception {

        _storage = new PropertyMockStorage();
        _conference = ConferenceFactory.createNewConference(_storage, "conference/test/", "", "My Test Conference", "Chair");
        _user = new User("Testuser");
        _conference.registerAuthor(_user, "", new HashMap<String,String>());
        _storage.ensurePathExists("test");
        
        _acl = new ACLManager(_conference);
    }
    
    public void testDummy() {
        _acl.getClass();        
    }
    
    public void testSetAuthorPagePermissions() throws Exception {
        String path = "test";
        
        _acl.setAuthorPagePermissions(_user, path);
    }
    /*
    public void testSetPaperRights() throws Exception {
        Paper paper = PaperFactory.submitPaper(_conference, _user, "TestPaper", "A test paper to test the ACLManager", new HashMap<String,String>());
        
        ArrayList<ConferenceState> allStates = new ArrayList<ConferenceState>();
        allStates.add(ConferenceState.BEFORE_PAPER_DEADLINE);
        allStates.add(ConferenceState.BEFORE_REVIEWER_ASSIGN_DEADLINE);
        allStates.add(ConferenceState.BEFORE_REVIEW_DEADLINE);
        allStates.add(ConferenceState.BEFORE_DISCUSSION_DEADLINE);
        allStates.add(ConferenceState.BEFORE_FINAL_DEADLINE);
        
        for(int t=0; t<allStates.size(); t++) {
            _conference.setState(allStates.get(t));
            _acl.setPaperRights(paper);
        }
    }
    */
}