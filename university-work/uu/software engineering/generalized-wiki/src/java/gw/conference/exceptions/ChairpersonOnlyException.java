package gw.conference.exceptions;

import gw.conference.Conference;

/**
 *  Thrown when the current user is not allowed to perform an action,
 *  but a chairperson is needed to do this instead.
 *   
 * @see gw.storage.InsufficientStorageAccessPermissionsException Similar, but for storage
 * */
public class ChairpersonOnlyException extends ConferenceException {
    private static final long serialVersionUID = 6319392423310092767L;

    public ChairpersonOnlyException(Conference conference) {
        super("Sorry, only the chair person of this conference (" + conference.getChairperson() + ") has access to perform this action.");
    }
}
