package gw.render.parsers;

import gw.render.parsers.ParseException;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;

import org.jdom.Element;
import org.jdom.input.SAXBuilder;

/**
 * This Parser can parse XML
 */
public class XMLParser implements Parser {
	private static XMLParser xmlParser;
    
    /**
     * @return The instance of this parser 
     */
	public static synchronized XMLParser getInstance(){
    	if (xmlParser == null) xmlParser = new XMLParser();
		return xmlParser;
    }
	
    /**
     * Parse the given XML content
     * 
     * @param reader
     *            The Reader that contains the input to parse
     * @return Element The parsed input
     * @throws ParseException
     */
    public Object parse(Reader reader) throws ParseException {
        Element result = null;
        try {
        	result = new SAXBuilder().build(reader).getRootElement();
        } catch (Exception e) {
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
