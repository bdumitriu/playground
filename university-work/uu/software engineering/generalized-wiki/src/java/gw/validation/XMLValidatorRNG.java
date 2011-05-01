package gw.validation;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;

import javax.xml.XMLConstants;

import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.thaiopensource.util.PropertyMap;
import com.thaiopensource.util.PropertyMapBuilder;
import com.thaiopensource.validate.ValidateProperty;
import com.thaiopensource.validate.ValidationDriver;
import com.thaiopensource.xml.sax.ErrorHandlerImpl;

/**
 *  Validates an XML document against a Relax NG schema using the JING lib.
 *  @author wrjpenar
 */
public class XMLValidatorRNG extends XMLValidatorAdaptor {
    
    /**
     * 
     * @param xml the xml file to be validated
     * @param schema the Relax NG schema file against which will be validated
     */
    public XMLValidatorRNG(InputStream xml, InputStream schema) {
        super(xml, schema);
    }

    /**
     * Starts validating.
     * 
     * @return true if validation succeeds
     * @throws ValidationException - Exceptions indicating what went wrong during the validation process in this case a SAXException.
     */
    public boolean validate() throws ValidationException
    {
        PropertyMapBuilder props = new PropertyMapBuilder();
        _errorBuffer = new StringWriter();
        ErrorHandler eh = new ErrorHandlerImpl(_errorBuffer);
        props.put(ValidateProperty.ERROR_HANDLER, eh);
           
        ValidationDriver driver = new ValidationDriver(props.toPropertyMap());
        InputSource xmlInputSource = null;
        InputSource rngInputSource = null;
        
        xmlInputSource = new InputSource(getXml());
        rngInputSource = new InputSource(getSchema());
                
        try {
            driver.loadSchema(rngInputSource);
            return driver.validate(xmlInputSource);
        } catch (SAXException e) {
            throw new ValidationException(e);
        } catch (IOException e) {
            throw new ValidationException(e);
        }     
    }
    /**
     * @return String indicating the Schema language
     * @see javax.xml.XMLConstants
     */
    public String getSchemaLanguage() {
        return XMLConstants.RELAXNG_NS_URI;
    }
}
