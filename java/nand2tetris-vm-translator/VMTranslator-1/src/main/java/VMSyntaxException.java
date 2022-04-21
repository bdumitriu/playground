public class VMSyntaxException extends RuntimeException {

    public VMSyntaxException(String message) {
        super("Syntax error: " + message);
    }
}
