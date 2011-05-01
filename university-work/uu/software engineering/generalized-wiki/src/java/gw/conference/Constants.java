package gw.conference;

/** A collection of constants used throughout the Conference application. */ 
public final class Constants {
    /*
     * MISC
     */
    public static final boolean DEBUG = true;
    
	private static final String PROPERTY_PREFIX = "conference-";
    private static final String SERVLET_PREFIX  = "/conference/";
    
    public static final ConferenceState INITIAL_STATE = ConferenceState.BEFORE_PAPER_DEADLINE;
    
    /*
     * DIRECTORY STRUCTURE
     */    
	public static final String AUTHORS_PREFIX = "Authors";
	public static final String PAPERS_PREFIX = "Papers";
	public static final String REVIEWS_PREFIX = "Reviews";
    public static final String REVIEW_FILE_PREFIX = "Review";    
    public static final String DISCUSSION_FILE = "Discussion";
    public static final String PAPER_RATINGS = "PaperRatings.xml";
    public static final String ADD_PAPER_FILE = "AddPaper";
    public static final String ADD_AUTHOR_FILE = "AddAuthor";
    public static final String ADD_RATING_FILE = "AddRating";
    public static final String ASSIGN_REVIEWER_FILE = "AssignReviewer";

    /*
     * PROPERTY NAMES (cannot contain spaces)
     */
    public static final String STATE = PROPERTY_PREFIX + "state";
    public static final String PAPER_SUBMIT_DEADLINE = "PaperSubmitDeadline";
	public static final String REVIEWER_ASSIGN_DEADLINE = "ReviewerAssignDeadline";
	public static final String REVIEW_SUBMIT_DEADLINE = "ReviewSubmitDeadline";
	public static final String DISCUSSION_SUBMIT_DEADLINE = "DiscussionSubmitDeadline";
	public static final String FINAL_DEADLINE = "FinalDeadline";
    public static final String FULL_TITLE = "FullTitle";
    public static final String PC_MEMBERS = "PCMembers";
    public static final String CHAIRPERSON = "Chairperson";
    public static final String WIKI_TITLE = "WikiTitle";
    public static final String ACCEPTED = "Accepted";
    public static final String RATING = "Rating";
    public static final String SUBMISSION_DATE = "SubmissionDate";
    public static final String AUTHORS = "Authors";
    public static final String REVIEWERS = "Reviewers"; 
    public static final String REVIEWER = "Reviewer";
    
    /** 
     * An array of all properties that can be applied to a conference
     * @see Conference#isValidProperty
     */
    public static final String[] CONFERENCE_PROPERTIES = new String[] {
        STATE, PAPER_SUBMIT_DEADLINE, REVIEWER_ASSIGN_DEADLINE, REVIEW_SUBMIT_DEADLINE,
        DISCUSSION_SUBMIT_DEADLINE, FINAL_DEADLINE, FULL_TITLE, PC_MEMBERS, CHAIRPERSON
    };
        

    /* 
     * SERVLET LOCATIONS 
     * If you change any of the "SERVLET" constants, you must also change the
     * corresponding <servlet-mapping> section in gw.xml!
     */
    // TODO: Make all servlet paths pascal/lower case
	public static final String NEW_CONFERENCE_SERVLET = SERVLET_PREFIX + "newConference";
    public static final String ADD_PAPER_SERVLET = SERVLET_PREFIX + "addPaper";
    public static final String ADD_AUTHOR_SERVLET = SERVLET_PREFIX + "addAuthor";
    public static final String ADD_REVIEW_SERVLET = SERVLET_PREFIX + "addReview";
    public static final String ASSIGN_REVIEWER_SERVLET = SERVLET_PREFIX + "assignReviewer";
    public static final String EDIT_CONFERENCE_SERVLET = SERVLET_PREFIX + "editConference";
    public static final String ACCEPT_PAPERS_SERVLET = SERVLET_PREFIX + "acceptPapers";
    public static final String ADD_RATING_SERVLET = SERVLET_PREFIX + "addRating";
}
