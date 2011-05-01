package gw.conference.actions;

import gw.conference.Constants;
import gw.conference.Paper;
import gw.conference.exceptions.ConferenceException;
import gw.conference.ui.ConferencePages;
import gw.storage.StorageException;
import java.io.IOException;

public class AssignReviewer extends ConferenceServlet {
	private static final long serialVersionUID = 62736876L;	

    @Override
    public void doRequest(ActionContext action) throws ConferenceException, StorageException, IOException {
        if(action.getParameters().isEmpty()) {
            // TODO: Remove redundant displayForm() once this form is written to the repository somewhere
            // should do action.writeInvalidAccessError(); instead
            displayForm(action);
        } else { 
            Paper paper = action.openPaperHere();
            String reviewerName = action.getParameters().get(Constants.REVIEWER);
            paper.assignReviewer(reviewerName);
 
            String smtpServer = getParamFromServlet("gw.emailsmtp");
            String emailFrom = getParamFromServlet("gw.emailfrom");
            
            // TODO: Don't ignore failure to send mail
            paper.sendReviewerMail(reviewerName, smtpServer, emailFrom, action);
        }      
    }

    private void displayForm(ActionContext action) throws ConferenceException, StorageException, IOException {
        Paper paper = action.openPaperHere();
        action.write(ConferencePages.getAssignReviewerPage(paper));        
    }
}
