package gw.conference.test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import junit.framework.TestCase;

import gw.conference.*;
import gw.conference.exceptions.*;
import gw.storage.Storage;
import gw.users.User;

public class ConferenceTest extends TestCase {
	private static final String TEST_PATH = "/conferences/test";
	
	private Storage _storage;
    private Conference _conference;
    
    public void setUp() throws Exception {
        _storage = new PropertyMockStorage();
        _conference = ConferenceFactory.createNewConference(_storage, TEST_PATH + "2", "My Test Conference", "", "Chair");
    }
	
	public void testCreateConference() throws Exception {
		Conference con = ConferenceFactory.createNewConference(_storage, TEST_PATH, "My Test Conference", "", "Chair");
        assertTrue(con.isValid());
        
        _storage.ensurePathExists(TEST_PATH);
		
        String state = new ConferenceDataAdapter(_storage, TEST_PATH).get(Constants.STATE);
        assertNotNull(state);        
	}
    
    public void testCreateExistingConference() throws Exception {
        ConferenceFactory.createNewConference(_storage, TEST_PATH, "My Test Conference", "", "Chairry the Chairperson");;
        
        // Test creating conference in same path (should fail)
        try {
            ConferenceFactory.createNewConference(_storage, TEST_PATH, "No can do", "", "Chair");
            fail(); // This should not happen
        } catch(ConferenceExistsException x) {
            // This should happen
        }        
    }
	
	public void testOpenConference() throws Exception {
		Conference con = ConferenceFactory.openConference(_storage, "", TEST_PATH + "2");
		
		assertTrue(con.isValid());
	}
    
    public void testAddProperties() throws Exception {
        Map<String,String> props = new HashMap<String,String>();
        props.put(Constants.FULL_TITLE, "Teh fool title");
        _conference.addProperties(props);
        assertTrue(_conference.getFullTitle().equals("Teh fool title"));
    }
    
    public void testGetPapers() throws Exception {
        assertTrue(_conference.getPapers().size() == 0);
    }
    
    public void testAddPropertiesFailing() throws Exception {
        try {
            Map<String,String> props = new HashMap<String,String>();
            props.put("Some invalid property name", "blah");
            _conference.addProperties(props);
            fail("Should throw an exception");
        } catch(InvalidPropertyException x) {
            assertTrue(true);
        }                    
    }
    
    public void testAddPropertiesFail2() throws Exception {
        try {
            Map<String,String> props = new HashMap<String,String>();
            props.put(Constants.FULL_TITLE, null);
            _conference.addProperties(props);
            fail("Should throw an exception");
        } catch(Exception x) {
            assertTrue(true);
        }                    
    }
    /*
    public void testSubmitPaper() throws Exception {
        User user = new User("Piet");
        _conference.registerAuthor(user, "", new HashMap<String,String>());
        PaperFactory.submitPaper(_conference, user, "BlahBlah", "Blah Blah", new HashMap<String,String>());
        try {
            PaperFactory.submitPaper(_conference, user, "BlahBlah", "Blah Blah", new HashMap<String,String>());
            fail("exception expected");
        } catch(PaperExistsException e) {
            assertTrue(true);
        }
    }
    */
    public void testSubmitPaperInvalidAuthor() throws Exception {
        try {
            User user = new User("Eelco");        
            PaperFactory.submitPaper(_conference, user, "Naughty", "Stuff", new HashMap<String,String>());
            fail("exception expected");
        } catch(Exception e) {
            assertTrue(true);
        }
    }
    
    public void testGetAndSetPCMembers() throws Exception {
        String[] list = _conference.getPCMembers();
        assertTrue(list.length == 0);
        
        ArrayList<String> pcMembers = new ArrayList<String>();
        pcMembers.add("pcMember1");
        pcMembers.add("pcMember2");
        _conference.setPCMembers(pcMembers);
        
        list = _conference.getPCMembers();
        assertEquals(list[0], pcMembers.get(0));
        assertEquals(list[1], pcMembers.get(1));
        
    }
 
    public void testRegisterAuthor() throws Exception {
        User user = new User("newUser");
        _conference.registerAuthor(user, "", new HashMap<String, String>());
        
        try{
            _conference.registerAuthor(user, "", new HashMap<String, String>());
            fail();
        }
        catch (AuthorExistsException e) {
            assertTrue(true);
        }
    }
    
    public void testGetAndSetPaperSubmitDeadline() throws Exception {
        _conference.setPaperSubmitDeadline(ConferenceDate.parse("25-10-2005"));
        assertEquals(_conference.getPaperSubmitDeadline(), ConferenceDate.parse("25-10-2005"));
    }
    
    public void testGetAndSetReviewerAssignDeadline() throws Exception {
        _conference.setReviewerAssignDeadline(ConferenceDate.parse("26-10-2005"));
        assertEquals(_conference.getReviewerAssignDeadline(), ConferenceDate.parse("26-10-2005"));
    }
    
    public void testGetAndSetReviewSubmitDeadline() throws Exception {
        _conference.setReviewSubmitDeadline(ConferenceDate.parse("27-10-2005"));
        assertEquals(_conference.getReviewSubmitDeadline(), ConferenceDate.parse("27-10-2005"));
    }
    
    public void testGetAndSetDiscussionSubmitDeadline() throws Exception {
        _conference.setDiscussionSubmitDeadline(ConferenceDate.parse("28-10-2005"));
        assertEquals(_conference.getDiscussionSubmitDeadline(), ConferenceDate.parse("28-10-2005"));
    }
    
    public void testGetAndSetFinalDeadline() throws Exception {
        _conference.setFinalDeadline(ConferenceDate.parse("26-10-2005"));
        assertEquals(_conference.getFinalDeadline(), ConferenceDate.parse("26-10-2005"));
    }
    
    public void testUpdateState() throws Exception {
        _conference.setPaperSubmitDeadline(ConferenceDate.getBeginOfTime());
        _conference.setReviewerAssignDeadline(ConferenceDate.getBeginOfTime());
        _conference.setReviewSubmitDeadline(ConferenceDate.getBeginOfTime());
        _conference.setDiscussionSubmitDeadline(ConferenceDate.getBeginOfTime());
        _conference.setFinalDeadline(ConferenceDate.getBeginOfTime());
        
        _conference.updateState();
        
        _conference.setFinalDeadline(ConferenceDate.getEndOfTime());
        _conference.updateState();
        
        _conference.setDiscussionSubmitDeadline(ConferenceDate.getEndOfTime());
        _conference.updateState();
        
        _conference.setReviewSubmitDeadline(ConferenceDate.getEndOfTime());
        _conference.updateState();
        
        _conference.setReviewerAssignDeadline(ConferenceDate.getEndOfTime());
        _conference.updateState();
        
        _conference.setPaperSubmitDeadline(ConferenceDate.getEndOfTime());
        _conference.updateState();
    }
}
