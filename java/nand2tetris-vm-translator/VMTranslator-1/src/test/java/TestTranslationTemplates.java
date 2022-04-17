import java.text.MessageFormat;

public enum TestTranslationTemplates {

    INSTANCE;

    private final static String TEST_TEMPLATE_PUSH_CONSTANT = """
            function-name = push
            function-type = constant
            function-args = {0}""";

    private final static String TEST_TEMPLATE_PUSH_TEMP = """
            function-name = push
            function-type = temp
            function-args = {0}""";

    private final static String TEST_TEMPLATE_PUSH_LOCAL = """
            function-name = push
            function-type = LCL
            function-args = {0}""";

    private final static String TEST_TEMPLATE_PUSH_ARGUMENT = """
            function-name = push
            function-type = ARG
            function-args = {0}""";

    private final static String TEST_TEMPLATE_PUSH_THIS = """
            function-name = push
            function-type = THIS
            function-args = {0}""";

    private final static String TEST_TEMPLATE_PUSH_THAT = """
            function-name = push
            function-type = THAT
            function-args = {0}""";

    private final static String TEST_TEMPLATE_POP_CONSTANT = """
            function-name = pop
            function-type = constant
            function-args = {0}""";

    private final static String TEST_TEMPLATE_POP_TEMP = """
            function-name = pop
            function-type = temp
            function-args = {0}""";

    private final static String TEST_TEMPLATE_POP_LOCAL = """
            function-name = pop
            function-type = LCL
            function-args = {0}""";

    private final static String TEST_TEMPLATE_POP_ARGUMENT = """
            function-name = pop
            function-type = ARG
            function-args = {0}""";

    private final static String TEST_TEMPLATE_POP_THIS = """
            function-name = pop
            function-type = THIS
            function-args = {0}""";

    private final static String TEST_TEMPLATE_POP_THAT = """
            function-name = pop
            function-type = THAT
            function-args = {0}""";

    public String getPushConstantTemplateFor(Object... args) {
        return getTemplateFor(TEST_TEMPLATE_PUSH_CONSTANT, args);
    }

    public String getPushTempTemplateFor(Object... args) {
        return getTemplateFor(TEST_TEMPLATE_PUSH_TEMP, args);
    }

    public String getPushLocalTemplateFor(Object... args) {
        return getTemplateFor(TEST_TEMPLATE_PUSH_LOCAL, args);
    }

    public String getPushArgumentTemplateFor(Object... args) {
        return getTemplateFor(TEST_TEMPLATE_PUSH_ARGUMENT, args);
    }

    public String getPushThisTemplateFor(Object... args) {
        return getTemplateFor(TEST_TEMPLATE_PUSH_THIS, args);
    }

    public String getPushThatTemplateFor(Object... args) {
        return getTemplateFor(TEST_TEMPLATE_PUSH_THAT, args);
    }

    public String getPopConstantTemplateFor(Object... args) {
        return getTemplateFor(TEST_TEMPLATE_POP_CONSTANT, args);
    }

    public String getPopTempTemplateFor(Object... args) {
        return getTemplateFor(TEST_TEMPLATE_POP_TEMP, args);
    }

    public String getPopLocalTemplateFor(Object... args) {
        return getTemplateFor(TEST_TEMPLATE_POP_LOCAL, args);
    }

    public String getPopArgumentTemplateFor(Object... args) {
        return getTemplateFor(TEST_TEMPLATE_POP_ARGUMENT, args);
    }

    public String getPopThisTemplateFor(Object... args) {
        return getTemplateFor(TEST_TEMPLATE_POP_THIS, args);
    }

    public String getPopThatTemplateFor(Object... args) {
        return getTemplateFor(TEST_TEMPLATE_POP_THAT, args);
    }

    private String getTemplateFor(String template, Object... args) {
        return MessageFormat.format(template, args);
    }
}
