package gw.validation;
import java.io.IOException;
import java.io.InputStream;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;


/**
 *  Validates an XML document against a W3C XML Schema using 
 *  using the default javax.xml lib as included in java 1.5
 *  @author wrjpenar
 *  @see javax.xml
 */
public class XMLValidatorWXS extends XMLValidatorAdaptor {
    /**
     * 
     * @param xml the xml file to be validated
     * @param schema the Relax NG schema file against which will be validated
     */
    public XMLValidatorWXS(InputStream xml, InputStream schema) {
        super(xml, schema);
    }
    /**
     * Starts validating.
     * 
     * @return true if validation succeeds
     * @throws ValidationException - Exceptions indicating what went wrong during the validation process in this case a SAXException.
     */
    public boolean validate() throws ValidationException {
        Schema schema = null;
        Document document = null;
        DocumentBuilder parser = null; 
        
        //here we read the xml file
        DocumentBuilderFactory docBuilder = DocumentBuilderFactory.newInstance();
        try {
            parser = docBuilder.newDocumentBuilder();
        } catch (ParserConfigurationException e2) {
            throw new ValidationException(e2);
        }
        try {
            document = parser.parse(getXml());
        } catch (SAXException e2) {
            throw new ValidationException(e2);
        } catch (IOException e2) {
            throw new ValidationException(e2);
        }

        //here we create the schema validator based on a W3C  XmlSchema
        SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
        // load a WXS schema, represented by a Schema instance
        Source schemaFile = new StreamSource(getSchema());
        try {
            schema = factory.newSchema(schemaFile);
        } catch (SAXException e1) {
            throw new ValidationException(e1);
        }
          
        // create a Validator instance, which can be used to validate an instance document
        Validator validator = schema.newValidator();
        try {
            //finally validate
            validator.validate(new DOMSource(document));
        } catch (SAXException e) {
            throw new ValidationException(e);
        } catch (IOException e) {
            throw new ValidationException(e);
        }
        
        return false;
    }
    /**
     * @return String indicating the Schema language
     * @see javax.xml.XMLConstants
     */
    public String getSchemaLanguage() {
        return XMLConstants.W3C_XML_SCHEMA_NS_URI;
    }
}
