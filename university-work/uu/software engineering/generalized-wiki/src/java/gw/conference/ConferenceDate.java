package gw.conference;


import gw.conference.exceptions.InvalidDateException;

import java.text.FieldPosition;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.GregorianCalendar;

/** A calendar with fixed formatting for parsing/outputting. 
 * @see #parse
 * @see #toString
 */
public class ConferenceDate extends GregorianCalendar {
	private static final long serialVersionUID = 23234324;
    
	// TODO: Change to MM-dd-yyyy or UTC datetime
    public static final String DATE_FORMAT = "dd-MM-yyyy";
	
	private final Date _date;
    
    private ConferenceDate(Date date) {
        _date = date;
        setTime(date);
    }
    
	/*
	 * Parses the date from a string
	 */
	
	public static ConferenceDate parse(String dateString) throws InvalidDateException {
        if(dateString == null) throw new IllegalArgumentException();
        
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
        
		Date date = sdf.parse(dateString, new ParsePosition(0));
	    
	    if(date == null) throw new InvalidDateException(dateString);

        return new ConferenceDate(date);
	}
    
    /**
     * @retun a date far in the future
     */
    public static ConferenceDate getEndOfTime() {
        return new ConferenceDate(new GregorianCalendar(3000, 11, 23).getTime());
    }

    /**
     * @return a date from the past
     */
    public static ConferenceDate getBeginOfTime() {
        return new ConferenceDate(new GregorianCalendar(1985, 06, 06).getTime());
    }

	public String toString() {
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
		FieldPosition pos = new FieldPosition(0);		
		StringBuffer sb = new StringBuffer();
		sdf.format(_date, sb, pos);
		return sb.toString();		
	}
}