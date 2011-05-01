package gw.validation;

/**
 * super interface for validators in the GW
 * 
 * @author wrjpenar
 */
public interface GwValidator {
    
    /**
     * Starts validating.
     * 
     * @return true if validation succeeds
     * @throws ValidationException - Exceptions indicating what went wrong during the validation process.
     * @throws ValidationWrapperException - wrapper around misc Exceptions, like for example IOExceptions
     */
	public boolean validate() throws ValidationException;
    
    /**
     * 
     * @return the results from the validation process in a String.
     */
    public String getValidationResults();
    
    /**
     * 
     * @return the results from the validation process in a String wrapped in a GWML document.
     */
    public String getValidationResultsGWML();
   
}
