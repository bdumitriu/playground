package gw.conference.actions;

import gw.conference.Conference;
import gw.conference.Constants;
import gw.conference.PaperRatings;
import gw.conference.Rating;
import gw.conference.RatingSet;
import gw.conference.exceptions.ConferenceException;
import gw.conference.exceptions.InvalidServletAccessException;
import gw.conference.ui.ConferencePages;
import gw.render.parsers.ParseException;
import gw.storage.StorageException;

import java.io.IOException;

import org.jdom.JDOMException;

public class AddRating extends ConferenceServlet {
    // Last class we made. *Whistles innoccently*
		
    private static final long serialVersionUID = -464532532L;
    @Override
    public void doRequest(ActionContext action) throws ConferenceException, StorageException, IOException, JDOMException, ParseException {
        if(action.getParameters().isEmpty())
            throw new InvalidServletAccessException();

        // with the path we figure out to which paper to add ratings.
        
        // Make the ratingset
        RatingSet ratingSet = new RatingSet();
        
        for(String parameter : action.getParameters().keySet()) {
            ratingSet.add(new Rating(parameter, action.getParameters().get(parameter)));
        }

        // Get the conference rating xml file 
        PaperRatings paperRatings = new PaperRatings();
        Conference con = action.openConferenceHere();
        String ratingPath = con.getPath()+Constants.PAPER_RATINGS;
        paperRatings.construct(action.getStorage().getFile(ratingPath));
        
        // Add the ratingSet
        paperRatings.add("paper", "reviewer", "date", ratingSet);

        // Write the xml file.
        ConferencePages.writeRatingFile(paperRatings, action.getStorage(), ratingPath);

        
        action.writeMessage("The new ratings were added succesfully!");
        action.close();
        
		
    }

    /* UNDONE: Unused code (in this revision, may be useful in the future again)
    // Just makes an example rating file
    private void newRatingFile(ActionContext action, String path) throws StorageException, IOException {   	
        ConferencePages.writeFile(action.getStorage(), path, testRatingFile(), GwConstants.CONFERENCE_RATINGS_MIME_TYPE);	
    	
    }
    
    private static String testRatingFile() {
        return  "";
    }

    //contextPath is during our project: gw/view/
    private void displayForm(ActionContext action, PrintWriter pw)  throws ConferenceException, StorageException, IOException {
        // Confirm conference existence (and get its title)
        // Conference conference = ConferenceFactory.openConference(action.getStorage(), action.getPathInfo());
        Paper paper = action.openPaperHere();        
        
        String submitURL = action.getContextPath() + Constants.ADD_REVIEW_SERVLET + action.getPathInfo();

        pw.write(ConferencePages.getAddRatingPage(paper));
    }
    */ 
}


