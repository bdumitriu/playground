package gw.conference.actions;

import gw.conference.Conference;
import gw.conference.exceptions.ChairpersonOnlyException;
import gw.conference.exceptions.ConferenceException;
import gw.conference.exceptions.InvalidServletAccessException;
import gw.storage.StorageException;

import java.io.IOException;

public class EditConference extends ConferenceServlet {
    private static final long serialVersionUID = -3134129640370563323L;

    public void doRequest(ActionContext action) throws ConferenceException, StorageException, IOException {
        if(action.getParameters().isEmpty())
            throw new InvalidServletAccessException();
               
        Conference con = action.openConferenceHere();
        
        if(!action.getUser().equals(con.getChairperson()))
            throw new ChairpersonOnlyException(con);
        
        con.addProperties(action.getParameters());
        action.sendViewRedirect();
    }
}
