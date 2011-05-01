/*
 * Created odn Oct 3, 2005
 */
package gw.conference.ui;

import java.util.LinkedHashMap;
import java.util.Map;

import gw.conference.Constants;

/**
 * Contains GWML default forms, written to the repository by the various factories.
 * 
 * @see ConferencePages
 * @see gw.conference.ConferenceFactory
 * @see gw.conference.PaperFactory
 */
class ConferenceForms {	

	public static String getNewConferenceForm(String pathInfo) {
        Map<String, String> fields = new LinkedHashMap<String, String>();
        
        fields.put("Full title", Constants.FULL_TITLE);
        fields.put("Submit deadline", Constants.PAPER_SUBMIT_DEADLINE);
        fields.put("Review assignment deadline", Constants.REVIEWER_ASSIGN_DEADLINE);
        fields.put("Review submit deadline", Constants.REVIEW_SUBMIT_DEADLINE);
        fields.put("Discussion deadline", Constants.DISCUSSION_SUBMIT_DEADLINE);
        fields.put("Final deadline", Constants.FINAL_DEADLINE);
        fields.put("Conference board members (seperate by comma's, no spaces)", Constants.PC_MEMBERS);
        
        return GwmlElements.getFormNiceHtml(fields, pathInfo, "Create conference");              
    }
    
    public static String getNewPaperForm(String pathInfo) {  
        return GwmlElements.getForm(pathInfo,
                GwmlElements.getTextFieldInParagraph("Full Title", Constants.FULL_TITLE) +
                GwmlElements.getTextFieldInParagraph("WikiTitle", Constants.WIKI_TITLE) +
                GwmlElements.getTextFieldInParagraph("Author(s)") +
                GwmlElements.getTextAreaInParagraph("Abstract") +
                GwmlElements.getButton("submit", "Add paper to conference"));
    }
    
    public static String getNewAuthorForm(String pathInfo) {
        return GwmlElements.getForm(pathInfo,
                GwmlElements.getTextFieldInParagraph(Constants.FULL_TITLE) 
                + GwmlElements.getTextFieldInParagraph("CustomField") 
                + GwmlElements.getButton("submit", "Register author"));
    }
    
    public static String getAssignReviewerForm(String pathInfo) {
        return GwmlElements.getForm(pathInfo,
                GwmlElements.getTextFieldInParagraph(Constants.REVIEWER) + 
                GwmlElements.getButton("submit", "Assign reviewer"));
    }
    
    public static String getNewReviewForm(String pathInfo) {
        return GwmlElements.getForm(pathInfo,
        		GwmlElements.getTextAreaInParagraph("Review") +
        		GwmlElements.getTextFieldInParagraph("Grade") +
                GwmlElements.getButton("submit", "Submit review"));
    }
}
