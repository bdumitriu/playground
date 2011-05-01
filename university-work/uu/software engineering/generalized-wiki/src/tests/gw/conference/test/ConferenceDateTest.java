/*
 * Created on Oct 4, 2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package gw.conference.test;

import java.util.Vector;

import gw.conference.ConferenceDate;
import gw.conference.exceptions.InvalidDateException;
import junit.framework.TestCase;

public class ConferenceDateTest extends TestCase {

		public void testParsingDates() throws Exception {
			// Moet in het formaat dd-mm-yyyy zijn.
			
			// Wrong dates
			Vector<String> wrong = new Vector<String>();
			wrong.add("34253242");
			wrong.add("dd-3--3--");

			
			for(String toTest : wrong) {
				try {
					ConferenceDate.parse(toTest);
					fail();
				} catch (InvalidDateException ide) {
					assertTrue(true);
				} 
			}
			
			// The good dates
			Vector<String> good = new Vector<String>();
			good.add("31-01-2005");
			good.add("01-02-2005");
			good.add("08-04-1985");
            good.add("23-76-2005");
            good.add("32-01-3200");
            good.add("1-8-2005");
			
			for(String toTest : good) {				
				ConferenceDate.parse(toTest);
			}			
		}
        
        public void testEndOfTime() throws Exception {
            ConferenceDate possibleLastUseOfGW = ConferenceDate.parse("22-12-2006");
            assertTrue(ConferenceDate.getEndOfTime().compareTo(possibleLastUseOfGW) > 0);
        }
        
        public void testBeginOfTime() throws Exception {
            ConferenceDate possibleFirstUseOfGW = ConferenceDate.parse("07-07-1985");
            assertTrue(ConferenceDate.getBeginOfTime().compareTo(possibleFirstUseOfGW) < 0);
        }
        
        public void testToString() {
            String text = ConferenceDate.getBeginOfTime().toString();
            assertEquals(text, "06-07-1985");
        }
}
