package gw.conference;

import gw.conference.actions.ActionContext;
import gw.conference.exceptions.InvalidDateException;
import gw.conference.exceptions.ReviewerAlreadyAssignedException;
import gw.conference.ui.ConferencePages;
import gw.storage.Storage;
import gw.storage.StorageException;
import gw.users.User;
import gw.users.UserDataAdapter;
import gw.users.UserDataAdapterFactory;
import gw.users.DataAdapterUserManagerFactory;
import gw.users.UserManager;
import gw.util.SmtpMailer;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Calendar;

/** 
 * Represents a Paper within a certain conference. 
 * Allows one to get/set properties of this paper.
 * Use openPaper or Conference.submit to construct an instance. 
 * Be sure to call updateACL() when appropriate after changing these properties.
 * 
 * @see PaperFactory
 * @see #updateACL
 * */
public class Paper {
    private final Conference _conference;
    private final ConferenceDataAdapter _data;
    private final String _wikiTitle;
	
	/** Returns a new instance of the paper class. Use a factory method to construct this.
	 * @see PaperFactory
	 */
    protected Paper(Conference conference, String paperRoot, String wikiTitle) throws StorageException {
        _data = new ConferenceDataAdapter(conference.getStorage(), paperRoot);
        _wikiTitle = wikiTitle;
        _conference = conference;
    }
    protected Paper(Conference conference, String paperRoot) throws StorageException {
        this(conference, paperRoot, getWikiTitle(paperRoot));
    }
    
    public static String getWikiTitle(String path) {
        int lastSlash = path.lastIndexOf("/", path.length() - 1);
        return path.substring(lastSlash + 1);
    }

    public void assignReviewer(String reviewerName) throws StorageException, ReviewerAlreadyAssignedException,
            IOException {

        List<String> newReviewers = new ArrayList<String>();
        String reviewPath, reviewFile;
        String reviewContent;
        Storage storage;
        
        /* iterate over existing reviewers;
         * check if new reviewer is not already assigned;
         * copy existing reviewers to new reviewers list;
         * add newly assigned reviewer to new reviewers list
         */
        for (String r : this.getReviewers()) {
            if (r.equals(reviewerName)) throw new ReviewerAlreadyAssignedException( "User " + reviewerName +
                    " has already been assigned to review paper " + this.getFullTitle());
            newReviewers.add(r);
        }
        newReviewers.add(reviewerName);
        this.setReviewers(newReviewers);
        
        reviewPath = getPath() + Constants.REVIEWS_PREFIX + "/";
        reviewFile = getReviewPath(reviewerName);
        reviewContent = ConferencePages.getReviewPageTemplate(this.getFullTitle());
        
        storage = _conference.getStorage();
        storage.ensurePathExists(reviewPath);        
        ConferencePages.write(storage, reviewFile, reviewContent);
        
        updateACL();       
    }
    
    /*
     * ACCESSORS
     */
    
    public Conference getConference() {
        return _conference;
    }
    public String getWikiTitle() {
        return _wikiTitle;
    }    
    /** Returns the associated path, with an ending slash. */
    public String getPath() {
        return _data.getPath();
    }    
    public void setAccepted(boolean published) throws StorageException {
        _data.set(Constants.ACCEPTED, Boolean.toString(published));
    }
    public boolean isAccepted() {
        String property = _data.get(Constants.ACCEPTED);
        return property == null ? false : Boolean.parseBoolean(property);
    }    
    public String getDiscussionPath() {
        return getPath() + Constants.DISCUSSION_FILE;
    }
    public int getRating() {
        String property = _data.get(Constants.RATING);
        return property == null ? -1 : Integer.parseInt(property);
    }
    public void setRating(int rating) throws StorageException {
        _data.set(Constants.RATING, Integer.toString(rating));
    }
    public String getFullTitle() {
        return _data.get(Constants.FULL_TITLE);
    }
    public void setTitle(String title) throws StorageException {
        _data.set(Constants.FULL_TITLE, title);
    }
    public Calendar getSubmissionDate() {
        String property = _data.get(Constants.SUBMISSION_DATE);
        try {
            return property == null ? ConferenceDate.getEndOfTime() : ConferenceDate.parse(property);
        } catch (InvalidDateException e) {
            return ConferenceDate.getEndOfTime();
        }
    }
    public void setSubmissionDate(ConferenceDate date) throws StorageException {
        if(date == null)
            _data.set(Constants.SUBMISSION_DATE, ConferenceDate.getEndOfTime().toString());
        else
            _data.set(Constants.SUBMISSION_DATE, date.toString());
    }    
    public String[] getAuthors() {
        return _data.getList(Constants.AUTHORS);
    }    
    public String[] getReviewers() {
        return _data.getList(Constants.REVIEWERS);
    }    
    public void setReviewers(List<String> reviewers) throws StorageException {
        _data.setList(Constants.REVIEWERS, reviewers);
    }    
    public void setAuthors(List<String> authors) throws StorageException {
        _data.setList(Constants.AUTHORS, authors);
    }
    public final void setAuthors(String author) throws StorageException {
        List<String> list = new ArrayList<String>();
        list.add(author);
        setAuthors(list);
    }    
    public String getReviewPath(String reviewerName) {
        int index = -1;
        String[] reviewers = getReviewers();

        for(int t=0; t<reviewers.length; t++) {
            if(reviewers[t].equals(reviewerName)) {
                index = t;
                t = reviewers.length;
            }
        }
        
        if(index == -1)
            throw new IllegalStateException("Reviewer must exist in list of reviewers");
        
        return getPath() + Constants.REVIEWS_PREFIX + "/" + Constants.REVIEW_FILE_PREFIX + index;         
    }

    /*
     * PUBLIC METHODS
     */
    
    /** Updates the access rights for this paper. */
    public void updateACL() throws StorageException {
        new ACLManager(_conference).setPaperRights(this);
        // TODO: should also set appropriate acls on each of the files in the Reviews directory
    }
    
    public void sendReviewerMail(String reviewerName, String smtpServer, String emailFrom, ActionContext action)
            throws IOException {

        Storage storage = action.getUnsecuredStorage();
        UserDataAdapter uda = UserDataAdapterFactory.getUserDataAdapter(storage);
        DataAdapterUserManagerFactory daumf = new DataAdapterUserManagerFactory(uda);
        UserManager userManager = daumf.getUserManager();
        User reviewer = userManager.getUser(reviewerName);
        
        // TODO: improve message subject and contents 
        SmtpMailer mailer = new SmtpMailer(smtpServer, reviewer.getEmail(), emailFrom,
                "Review Paper", "Please complete the paper review at: " + getReviewPath(reviewerName));
        mailer.send();
    }
}
