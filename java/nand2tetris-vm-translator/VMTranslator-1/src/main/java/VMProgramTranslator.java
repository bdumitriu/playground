public class VMProgramTranslator {

    private final VMCommandTranslator translator = new VMCommandTranslator();

    private final StringBuilder stringBuilder = new StringBuilder();

    public void translateAndAppend(String line) {
        final String translatedLine = translator.translate(line);
        if (!translatedLine.isBlank()) {
            stringBuilder.append("// '");
            stringBuilder.append(line);
            stringBuilder.append("'");
            stringBuilder.append(System.lineSeparator());
            stringBuilder.append(translatedLine);
            stringBuilder.append(System.lineSeparator());
        }
    }

    public String getProgram() {
        return stringBuilder.toString();
    }
}
