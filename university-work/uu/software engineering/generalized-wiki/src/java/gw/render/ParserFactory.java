package gw.render;

import gw.render.parsers.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * This class returns a Parser given a mime-type
 */
public class ParserFactory {

    private ArrayList entries;
    
    public ParserFactory() {
    	entries = new ArrayList();
    }
    
    public void register(String inputContentType, String outputContentType, Parser parser) {
    	entries.add(new ParserEntry(inputContentType, outputContentType, parser));
    }

    /**
     * Return a Parser given a content-type An Exception is thrown if the
     * content-type is unknown
     * 
     * @param contentType
     *            The mimeType on which a parser is selected
     * @throws ParserFactoryException
     */
    public Parser lookupByType(String inputContentType, String outputContentType) { 	
    	Parser result = DefaultParser.getInstance();
    	ParserEntry entry;
        for (int i=0;i< entries.size();i++){
        	entry = ((ParserEntry) entries.get(i));
        	if (entry.getInputContentType().equals(inputContentType)
        			&& entry.getOutputContentType().equals(outputContentType))
        		result = entry.getParser();
        }     
        return result;
    }
}

class ParserEntry{
	private String inputContentType;
	private String outputContentType;
	private Parser parser;
	
	ParserEntry(String inputContentType, String outputContentType, Parser parser){
		this.inputContentType = inputContentType;
		this.outputContentType =  outputContentType;
		this.parser = parser;
	}
	
	public String getInputContentType() {
		return inputContentType;
	}
	public void setInputContentType(String inputContentType) {
		this.inputContentType = inputContentType;
	}
	public String getOutputContentType() {
		return outputContentType;
	}
	public void setOutputContentType(String outputContentType) {
		this.outputContentType = outputContentType;
	}
	public Parser getParser() {
		return parser;
	}
	public void setParser(Parser parser) {
		this.parser = parser;
	}
}
