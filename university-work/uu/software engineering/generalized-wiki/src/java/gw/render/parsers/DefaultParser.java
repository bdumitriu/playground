package gw.render.parsers;

import gw.render.parsers.ParseException;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.BufferedReader;
import java.io.StringReader;

import org.jdom.Element;
import org.jdom.input.SAXBuilder;

/**
 * This Parser can parse arbitrary stuff
 */
public class DefaultParser implements Parser {

	private static DefaultParser defaultParser;
    
    /**
     * @return The instance of this parser 
     */
	public static synchronized DefaultParser getInstance(){
		if (defaultParser==null) defaultParser = new DefaultParser();
    	return defaultParser;
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
        Element result = null;
        StringBuilder input = new StringBuilder("<root xmlns=\"http://www.cs.uu.nl/wiki/Gw\">" );                
        
        try 
        {
        
	        BufferedReader breader = new BufferedReader(reader);
	        String read = breader.readLine();
	        while( read != null ){
	        	input.append(read);
	        	read = breader.readLine();
	        }
	        input.append("</root>");
	               
	        result = new SAXBuilder().build(new StringReader(input.toString())).getRootElement();
        } 
        catch (Exception e) {
            throw new ParseException(e);
        }
        return result;
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
