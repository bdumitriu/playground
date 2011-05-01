package gw.validation;

import java.io.InputStream;
import java.io.StringWriter;

import org.jdom.Element;
import org.jdom.output.XMLOutputter;

/**
 * A Convenience class.
 * @author wrjpenar
 * @see gw.validation.XMLValidator.java
 */

public abstract class XMLValidatorAdaptor implements XMLValidator {
    private InputStream _xml, _schema;
    protected StringWriter  _errorBuffer;
    
    public XMLValidatorAdaptor(InputStream xml, InputStream schema)
    {
        this._xml = xml;
        this._schema = schema;
    }
    
    public InputStream getSchema() {
        return _schema;
    }

    public void setSchema(InputStream _schema) {
        this._schema = _schema;
    }

    public InputStream getXml() {
        return _xml;
    }

    public void setXml(InputStream _xml) {
        this._xml = _xml;
    }

    public String getValidationResults() {
        return _errorBuffer.getBuffer().toString();
    }
    
    public String getValidationResultsGWML()
    {
    	Element document = new Element("document");
    	Element section = new Element("section");
    	Element title = new Element("title");
    	Element para = new Element("para");
    	Element pre = new Element("pre");
    	
    	document.addContent(section);
    	section.addContent(title);
    	section.addContent(para);
    	title.addContent("Validation Report");
    	para.addContent(pre);
    	pre.addContent(_errorBuffer.getBuffer().toString());
    	
    	XMLOutputter output = new XMLOutputter();
    	return output.outputString(document);
    }
}
