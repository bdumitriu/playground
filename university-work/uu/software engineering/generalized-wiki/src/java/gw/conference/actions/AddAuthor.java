package gw.conference.actions;

import gw.GwConstants;
import gw.conference.*;
import gw.conference.exceptions.ConferenceException;
import gw.conference.exceptions.InvalidServletAccessException;
import gw.storage.StorageException;
import gw.users.User;

import java.io.IOException;
import java.util.Map;

public class AddAuthor extends ConferenceServlet {
    private static final long serialVersionUID = -4565851614025462661L;

    @Override
    public void doRequest(ActionContext action) throws ConferenceException, StorageException, IOException {
        if(action.getParameters().isEmpty())
            throw new InvalidServletAccessException();
        
        Conference conference = action.openConferenceHere();
        User user = action.getUser();
        Map<String,String> parameters = action.getParameters();
        String fullTitle = parameters.remove(Constants.FULL_TITLE); 
    
        conference.registerAuthor(user, fullTitle, parameters);
        
        action.sendViewRedirect(action.getPathInfo() + "/" + Constants.AUTHORS_PREFIX + "/" + user.getId() + "/" + GwConstants.INDEX_PAGE_NAME );
    }

}
