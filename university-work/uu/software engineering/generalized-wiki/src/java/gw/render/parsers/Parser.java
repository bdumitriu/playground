package gw.render.parsers;

import gw.render.parsers.ParseException;

import java.io.InputStream;
import java.io.Reader;

import org.jdom.Element;

/**
 * Interface for a Parser
 */
public interface Parser {

	/**
	 * Parse the given content
	 * 
	 * @param reader 	The Reader that contains the input to parse
	 * @return Element	The parsed input
	 * @throws ParseException
	 */
	public Object parse(Reader reader) throws ParseException;
	
	
	
	/**
	 * Parse the given content
	 * 
	 * @param inputStream 	The InputStream that contains the input to parse
	 * @return Element		The parsed input
	 * @throws ParseException
	 */
	public Object parse(InputStream inputStream) throws ParseException;
	
	/**
	 * Parse the given content
	 * 
	 * @param string 	The String that contains the input to parse
	 * @return Element	The parsed input
	 * @throws ParseException
	 */
	public Object parse(String string) throws ParseException;
	
}
