package gw.conference.actions;

import gw.GwConstants;
import gw.conference.*;
import gw.conference.exceptions.*;
import gw.conference.ui.ConferencePages;
import gw.storage.StorageException;

import java.io.IOException;
import java.util.Map;

/** Servlet creating a new conference. */
public class NewConference extends ConferenceServlet {
	private static final long serialVersionUID = 3847873478L;
	
    @Override
    public void doRequest(ActionContext action) throws ConferenceException, StorageException, IOException {
        Map<String,String> parameters = action.getParameters();
        
        if(parameters.isEmpty()) {
            String submitURL = action.getContextPath() + Constants.NEW_CONFERENCE_SERVLET + action.getPathInfo();
            // TODO: The below should be proper GWML and be processed by a stylesheet 
            action.writeGwml(ConferencePages.getNewConferencePage(submitURL));
        } else {
			String fullTitle = parameters.get(Constants.FULL_TITLE);
            String contextPath = action.getContextPath();
			parameters.remove(Constants.FULL_TITLE);
			Conference con = ConferenceFactory.createNewConference(action.getStorage(), action.getPathInfo(), contextPath, fullTitle, action.getUser().getId());
			con.addProperties(parameters);
            
            action.sendViewRedirect(action.getPathInfo() + "/" + GwConstants.INDEX_PAGE_NAME);
        }
	}
}