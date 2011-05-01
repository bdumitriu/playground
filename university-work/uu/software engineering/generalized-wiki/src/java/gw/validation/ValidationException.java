package gw.validation;


public class ValidationException extends Exception{

    public ValidationException(Throwable e) {
        super(e);
    }
    
    public ValidationException(String message){ 
        super(message);
    }
    
    public ValidationException(){
        super();
    }
    
    public ValidationException(String m, Throwable t)
    {
        super(m, t);
    }
}
