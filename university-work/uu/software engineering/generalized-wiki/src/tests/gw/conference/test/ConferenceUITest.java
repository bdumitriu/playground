package gw.conference.test;

import java.util.HashMap;

import gw.conference.Conference;
import gw.conference.ConferenceFactory;
import gw.conference.Paper;
import gw.conference.PaperFactory;
import gw.conference.ui.ConferencePages;
import gw.render.parsers.XMLParser;
import gw.users.User;
import junit.framework.TestCase;

public class ConferenceUITest extends TestCase {
    Conference _conference;
    Paper _paper;
    User _user;
    
    public void setUp() throws Exception {
        _conference = ConferenceFactory.createNewConference(new PropertyMockStorage(), "/blah", "", "Full title", "MrBean");
        _user = new User("MrBean");
        _conference.registerAuthor(_user, "", new HashMap<String,String>());
        _paper = PaperFactory.submitPaper(_conference, _user, "WickedTitle", " Fool title", new HashMap<String,String>()); 
    }
    
    public void testConferenceWelcome() throws Exception {
        assertWellformed(ConferencePages.getConferenceWelcomePage(_conference));
    }
    
    public void testAddAuthorPage() throws Exception {
        assertWellformed(ConferencePages.getAddAuthorPage(_conference));
    }
    
    public void testAddPaperPage() throws Exception {
        assertWellformed(ConferencePages.getAddPaperPage(_conference));
    }
    
    public void testAddRatingPage() throws Exception {
        assertWellformed(ConferencePages.getAddRatingPage(_paper));
    }
    
    public void testDiscussionPage() throws Exception {
        assertWellformed(ConferencePages.getDiscussionPage());
    }


    
    public void testNewConferencePage() throws Exception {
        assertWellformed(ConferencePages.getNewConferencePage("/conference"));
    }
    
    public void testNewPaperPage() throws Exception {

        assertWellformed(ConferencePages.getNewPaperPage(_paper, _user.getId(), new HashMap<String,String>()));


    }
    
    private void assertWellformed(String page) throws Exception {
        try {
            XMLParser.getInstance().parse(page);
            assertTrue(true);
        } catch(Exception x) {
            System.out.println(x.toString());
            System.out.println(page);
            throw x;
        }
    }
}
