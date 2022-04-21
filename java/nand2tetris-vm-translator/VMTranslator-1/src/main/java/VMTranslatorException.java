public class VMTranslatorException extends RuntimeException {

    public VMTranslatorException(String message) {
        super("Translation error: " + message);
    }
}
