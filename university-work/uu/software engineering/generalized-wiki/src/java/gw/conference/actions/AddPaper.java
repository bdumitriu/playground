package gw.conference.actions;

import gw.GwConstants;
import gw.conference.*;
import gw.conference.exceptions.ConferenceException;
import gw.conference.exceptions.InvalidServletAccessException;
import gw.render.parsers.ParseException;
import gw.storage.StorageException;

import java.io.IOException;
import java.util.Map;

public class AddPaper extends ConferenceServlet {
	private static final long serialVersionUID = 8739874987L;
	
	public void doRequest(ActionContext action) throws ConferenceException, StorageException, IOException, ParseException { 
        Map<String,String> parameters = action.getParameters();
        
        if(parameters.isEmpty()) 
            throw new InvalidServletAccessException();
                
        Conference conference = action.openConferenceHere();
        String wikiTitle = parameters.remove(Constants.WIKI_TITLE);               
        String fullTitle = parameters.remove(Constants.FULL_TITLE);              
        
        Paper paper = PaperFactory.submitPaper(conference, action.getUser(), wikiTitle, fullTitle, parameters);
        
        action.sendViewRedirect(paper.getPath() + "/" + GwConstants.INDEX_PAGE_NAME);
    }
}