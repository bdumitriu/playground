/*
 * Created on Oct 3, 2005

 */
package gw.conference.ui;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.Map;

import gw.GwConstants;
import gw.conference.Conference;
import gw.conference.Constants;
import gw.conference.PaperRatings;
import gw.conference.Paper;
import gw.conference.ui.GwmlElements;
import gw.storage.Storage;
import gw.storage.StorageException;

/**
 * Generates some of the default content and forms for the Conference.
 * @author grjzwaan
 */
public class ConferencePages { 
    // NOTE: This should probably be moved to seperate files,
    // but time constraints are killing us...
	
    /* 
     * Writing the page
     */
    
    public static void write(Storage storage, String path, String content) throws StorageException, IOException {
        writeFile(storage, path, content, GwConstants.GWML_MIME_TYPE );
    }
    
    public static void writeFile(Storage storage, String file, String content, String contentType) throws StorageException, IOException {
        OutputStream stream = null;
        try {
            stream = storage.storeFile(file);      
            OutputStreamWriter output = new OutputStreamWriter(stream);
            
            output.write(content);
            
            output.flush();
            output.close(); // Need to close before setting property
            
            storage.setProperty(file, GwConstants.CONTENT_TYPE_PROPERTY, contentType, false);
        } finally {
            if(stream != null) stream.close();
        }
    }
    
    public static void writeRatingFile(PaperRatings paperRatings, Storage storage, String path) throws StorageException, IOException {
        OutputStream stream = null;
        try {
            stream = storage.storeFile(path);      
            OutputStreamWriter output = new OutputStreamWriter(stream);
            
            paperRatings.writeToStream(output);
            
            output.flush();
            output.close(); // Need to close before setting property
            
            storage.setProperty(path, GwConstants.CONTENT_TYPE_PROPERTY, GwConstants.CONFERENCE_RATINGS_MIME_TYPE, false);
        } finally {
            if(stream != null) stream.close();
        }       
    }

    
    /*
     * The pages
     */
    public static String getNewAuthorPage(Conference conference, String author, Map<String, String> fields) {
        return GwmlElements.getSingleParagraphPage(
                author,
                GwmlElements.getParagraph(GwmlElements.makeLink("Submit a paper", conference.getContextPath() + "/view" + conference.getPath() + Constants.ADD_PAPER_FILE))
                + GwmlElements.table(fields)
                );
    }
    

    public static String getNewPaperPage(Paper paper, String author, Map<String, String> fields) {
        String contextPath = paper.getConference().getContextPath();


        return GwmlElements.getSingleParagraphPage(

                paper.getFullTitle(),
                GwmlElements.getSection("Welcome", "...to this Paper submitted by "
                        + GwmlElements.makeLink(author, contextPath + paper.getConference().getPath() + Constants.AUTHORS_PREFIX + "/" + author)
                        + GwmlElements.table(fields)
                )
                + GwmlElements.getSection("Actions",
                        GwmlElements.getParagraph(GwmlElements.makeLink("Assign reviewer", Constants.ASSIGN_REVIEWER_FILE))
                        +  GwmlElements.getParagraph(GwmlElements.makeLink("Add rating", Constants.ADD_RATING_FILE))
                        // TODO: + GwmlElements.makeLink("Reviewer Discussion", Constants.DISCUSSION_FILE)
                ));
    }
    
	public static String getNewConferencePage(String pathInfo) {
		return GwmlElements.getSingleParagraphPage(
				"Create a new conference",
                GwmlElements.getParagraph("Path: " + pathInfo)
				+ ConferenceForms.getNewConferenceForm(pathInfo)
				);
	}

    public static String getReviewPageTemplate(String paperTitle) {
        return GwmlElements.getSingleParagraphPage(
                "Review of paper " + paperTitle + ".", 
                GwmlElements.getParagraph("Please enter your review of the paper here.")
                );
    }
    	
    public static String getAddPaperPage(Conference conference) {
        String contextPath = conference.getContextPath();
        return GwmlElements.getSingleParagraphPage(
                "Add a paper to " + conference.getFullTitle(), 
                ConferenceForms.getNewPaperForm(contextPath + Constants.ADD_PAPER_SERVLET + "/" + conference.getPath())
                );
    }
    
    public static String getAddAuthorPage(Conference conference) {
        String contextPath = conference.getContextPath();
        return GwmlElements.getSingleParagraphPage(
                "Add an author to " + conference.getFullTitle(), 
                ConferenceForms.getNewAuthorForm(contextPath + Constants.ADD_AUTHOR_SERVLET + "/" + conference.getPath())
                );
    }
    
    public static String getAssignReviewerPage(Paper paper) {
        String contextPath = paper.getConference().getContextPath();
        return GwmlElements.getSingleParagraphPage(
                "Assign a reviewer to the paper " + paper.getFullTitle(), 
                ConferenceForms.getAssignReviewerForm(contextPath + Constants.ASSIGN_REVIEWER_SERVLET + "/" + paper.getPath())
                );
    }
    
	public static String getDiscussionPage() {
		return GwmlElements.getSingleParagraphPage(
                "", "Nothing to see here, move along (or discuss using \"edit\" below).");
	}
    
    public static String getAddRatingPage(Paper paper) {
        String contextPath = paper.getConference().getContextPath();
        return GwmlElements.getSingleParagraphPage(
                "Add a rating to " + paper.getFullTitle(),
                GwmlElements.getForm(contextPath + Constants.ADD_RATING_SERVLET + "/" + paper.getPath(),
                        GwmlElements.getTextFieldInParagraph("Style") 
                      + GwmlElements.getTextFieldInParagraph("Anders")
                      + GwmlElements.getButton("submit", "Rate it!")));
    }
	
	public static String getConferenceWelcomePage(Conference conference) {
        String contextPath = conference.getContextPath();
		return GwmlElements.getPage(
				conference.getFullTitle(),
                GwmlElements.getSection("Welcome", 
                    "Welcome to this Generalized Wiki Conference. To read more about the GW system, have a look here:"
                    + GwmlElements.makeLink("GW Project Homepage", "http://www.cs.uu.nl/wiki/Gw") + " and the "
                    + GwmlElements.makeLink("Conference Project Wiki", "http://www.cs.uu.nl/wiki/Gw/ConferenceManagement") + ".")
                + GwmlElements.getSection("Authors",

                    "Paper authors can (1) register themselves as GW users (see the Login page below), and then (2) "
                    + GwmlElements.makeLink("add themselves as an author", Constants.ADD_AUTHOR_FILE)

                    + ", and (3) " + GwmlElements.makeLink("submit a paper", Constants.ADD_PAPER_FILE)
                    + ". Be sure to do a commit after every change you make. Read the GW Manual for more details."
                )
                + GwmlElements.getSection("Reviewers",
                    "All reviewers will be emailed with a link and details about reviewing."
                )
                + GwmlElements.getSection("Administration",
                    "Conference administration can be done through the "
                    + GwmlElements.makeLink("configuration page", contextPath + Constants.EDIT_CONFERENCE_SERVLET + conference.getPath())
                    + " and the " + GwmlElements.makeLink("paper acceptance page", contextPath + Constants.ACCEPT_PAPERS_SERVLET + conference.getPath()) 
                    + "."
                )
                + GwmlElements.getSection("General",
                    GwmlElements.makeLink("List of all papers", Constants.PAPERS_PREFIX)
                )
		); // TODO: Link to Constants.PAPER_RATINGS
	}

}
