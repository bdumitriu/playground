package gw.validation;
import java.io.InputStream;

/**
 * Interface for XML validators
 * 
 * @author wrjpenar
 */
public interface XMLValidator extends GwValidator {
    /**
     * Starts validating.
     * 
     * @return true if validation succeeds
     * @throws ValidationException - Exceptions indicating what went wrong during the validation process in this case a SAXException.
     * @throws ValidationWrapperException - wrapper around misc Exceptions, like for example IOExceptions
     */
    public boolean validate() throws ValidationException;
    
    /**
     * Sets the current schema
     * 
     * @param _schema the schema against which the document will be validated
     */
    public void setSchema(InputStream _schema);
    /** 
     * Get the current schema
     * 
     * @return returns the current Schema against which the document will be validated
     */
    public InputStream getSchema ();
    /** 
     * Get the current xml file
     * 
     * @return returns the current xml file that will be validated
     */
    public InputStream getXml();
    
    /**
     * Set the current xml file
     * 
     * @param _xml the xml which will be validated
     */
    public void setXml(InputStream _xml);
    
    /**
     * @return String indicating the Schema language
     * @see javax.xml.XMLConstants
     */
    public String getSchemaLanguage();
}
