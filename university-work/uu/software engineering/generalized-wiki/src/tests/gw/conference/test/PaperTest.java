package gw.conference.test;

import java.util.Arrays;
import java.util.HashMap;

import gw.conference.Conference;
import gw.conference.ConferenceDate;
import gw.conference.ConferenceFactory;
import gw.conference.Constants;
import gw.conference.Paper;
import gw.conference.PaperFactory;
import gw.storage.Storage;
import gw.users.User;

import junit.framework.TestCase;

public class PaperTest extends TestCase{
    Paper _paper;
    String[] _authors = new String[] { "author1", "author2" };
    String[] _reviewers = new String[] { "reviewer1", "reviewer2" };
    String _path;
    String _title;
    Conference _conference;
    
    public void setUp() throws Exception {
        _title = "Title";
        _path = "conference/test/";
        
        Storage storage = new PropertyMockStorage();
        _conference = ConferenceFactory.createNewConference(storage, _path, "My Test Conference", "", "Chair");

        _conference.registerAuthor(new User(_authors[0]), "", new HashMap<String,String>());
        _conference.registerAuthor(new User(_authors[1]), "", new HashMap<String,String>());
        _paper = PaperFactory.submitPaper(_conference, new User(_authors[0]), _title, _title, new HashMap<String,String>());
        _path += Constants.PAPERS_PREFIX + "/" + _title + "/";
        _paper.setTitle(_title);
        _paper.setAuthors(Arrays.asList(_authors));
        _paper.setReviewers(Arrays.asList(_reviewers));
    }
    
    public void testGetAuthors() {
        Arrays.equals(_authors, _paper.getAuthors());
    }
    
    public void testGetReviewers() {
        Arrays.equals(_reviewers, _paper.getReviewers());
    }
    
    public void testGetDiscussionPath() {
        assertEquals(_paper.getDiscussionPath(), _path + Constants.DISCUSSION_FILE);
    }
    
    public void testgetPaperPath() {
        assertEquals(_paper.getPath(), _path);
    }
    
    public void testPublishSetAndGet() throws Exception {
        assertFalse(_paper.isAccepted());
        _paper.setAccepted(true);
        assertTrue(_paper.isAccepted());
        _paper.setAccepted(false);
        assertFalse(_paper.isAccepted());
    }
    
    public void testRatingGetAndSet() throws Exception {
        assertTrue(_paper.getRating()==-1);
        _paper.setRating(5);
        assertTrue(_paper.getRating()==5);
    }
    
    public void testGetReviewPath() {
        String correctPath = _path + Constants.REVIEWS_PREFIX + "/" + Constants.REVIEW_FILE_PREFIX + "1";
        assertEquals(_paper.getReviewPath("reviewer2"), correctPath);
        
        try {
            _paper.getReviewPath("ThisNameWilNeverBeInThere34995683");
            fail(); // This should not happen
        }
        catch(Exception e) {
            // This should happen
            assertTrue(true);
        }  
    }
    
    public void testGetSubmissionDate() throws Exception {
        assertEquals(_paper.getSubmissionDate(), ConferenceDate.getEndOfTime());
        ConferenceDate newDate = ConferenceDate.parse("06-06-1985");
        _paper.setSubmissionDate(newDate);
        assertEquals(_paper.getSubmissionDate(), newDate);
        _paper.setSubmissionDate(null);
        assertEquals(_paper.getSubmissionDate(), ConferenceDate.getEndOfTime());
    }
    
    public void testGetTitle() throws Exception {
        assertTrue(_paper.getFullTitle().equals(_title));
    }
}