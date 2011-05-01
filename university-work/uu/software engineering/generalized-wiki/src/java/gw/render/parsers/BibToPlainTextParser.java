package gw.render.parsers;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;


public class BibToPlainTextParser implements Parser{
	private static BibToPlainTextParser bibToPlainParser;
    
    /**
     * @return The instance of this parser 
     */
	public static synchronized BibToPlainTextParser getInstance(){
		if (bibToPlainParser==null) bibToPlainParser = new BibToPlainTextParser();
    	return bibToPlainParser;
    }
    
    /**
     * Parse the given arbitrary content
     * 
     * @param reader
     *            The Reader that contains the input to parse
     * @return Element The parsed input
     * @throws ParseException
     */
    public Object parse(Reader reader) throws ParseException {
        String ouput ="";
        try {
        	BufferedReader breader = new BufferedReader(reader);
        	String read = breader.readLine();
        	while( read != null ){
        		ouput += read;
        		read = breader.readLine();
        	}
        } catch (Exception e) {
            throw new ParseException(e);
        }
        return ouput;
    }

    /**
     * Parse the given XML content
     * 
     * @param inputStream
     *            The InputStream that contains the input to parse
     * @return Element The parsed input
     * @throws ParseException
     */
    public Object parse(InputStream inputStream) throws ParseException {
        return parse(new InputStreamReader(inputStream));
    }

    /**
     * Parse the given XML content
     * 
     * @param string
     *            The String that contains the input to parse
     * @return Element The parsed input
     * @throws ParseException
     */
    public Object parse(String string) throws ParseException {
        return parse(new StringReader(string));
    }
}
