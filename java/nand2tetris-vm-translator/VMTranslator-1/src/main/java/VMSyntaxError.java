public class VMSyntaxError extends RuntimeException {

    public VMSyntaxError(String message) {
        super("Syntax error: " + message);
    }
}
