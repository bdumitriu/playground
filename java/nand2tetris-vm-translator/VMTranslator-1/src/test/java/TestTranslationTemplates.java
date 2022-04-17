import java.text.MessageFormat;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public enum TestTranslationTemplates {

    INSTANCE;

    private final static String TEST_TEMPLATE_PUSH_CONSTANT = """
            @{0}
            D=A
            @SP
            A=M
            M=D
            @SP
            M=M+1""";

    private final static String TEST_TEMPLATE_PUSH_TEMP = """
            @R{0}
            D=M
            @SP
            A=M
            M=D
            @SP
            M=M+1""";

    private final static String TEST_TEMPLATE_PUSH_LOCAL = """
            @LCL
            D=M
            @R13
            M=D
            @{0}
            D=A
            (PUSH_LCL.{1}.LOOP)
            D=D-1
            @PUSH_LCL.{1}.END
            D;JLT
            @R13
            M=M+1
            @PUSH_LCL.{1}.LOOP
            D;JGE
            (PUSH_LCL.{1}.END)
            @R13
            A=M
            D=M
            @SP
            A=M
            M=D
            @SP
            M=M+1""";

    private final static String TEST_TEMPLATE_PUSH_ARGUMENT = """
            @ARG
            D=M
            @R13
            M=D
            @{0}
            D=A
            (PUSH_ARG.{1}.LOOP)
            D=D-1
            @PUSH_ARG.{1}.END
            D;JLT
            @R13
            M=M+1
            @PUSH_ARG.{1}.LOOP
            D;JGE
            (PUSH_ARG.{1}.END)
            @R13
            A=M
            D=M
            @SP
            A=M
            M=D
            @SP
            M=M+1""";

    private final static String TEST_TEMPLATE_PUSH_THIS = """
            @THIS
            D=M
            @R13
            M=D
            @{0}
            D=A
            (PUSH_THIS.{1}.LOOP)
            D=D-1
            @PUSH_THIS.{1}.END
            D;JLT
            @R13
            M=M+1
            @PUSH_THIS.{1}.LOOP
            D;JGE
            (PUSH_THIS.{1}.END)
            @R13
            A=M
            D=M
            @SP
            A=M
            M=D
            @SP
            M=M+1""";

    private final static String TEST_TEMPLATE_PUSH_THAT = """
            @THAT
            D=M
            @R13
            M=D
            @{0}
            D=A
            (PUSH_THAT.{1}.LOOP)
            D=D-1
            @PUSH_THAT.{1}.END
            D;JLT
            @R13
            M=M+1
            @PUSH_THAT.{1}.LOOP
            D;JGE
            (PUSH_THAT.{1}.END)
            @R13
            A=M
            D=M
            @SP
            A=M
            M=D
            @SP
            M=M+1""";

    private final static String TEST_TEMPLATE_PUSH_STATIC = """
            @R{0}
            D=M
            @SP
            A=M
            M=D
            @SP
            M=M+1""";

    private final static String TEST_TEMPLATE_PUSH_POINTER = """
            @R{0}
            D=M
            @SP
            A=M
            M=D
            @SP
            M=M+1""";

    private final static String TEST_TEMPLATE_POP_TEMP = """
            @SP
            AM=M-1
            D=M
            @R{0}
            M=D""";

    private final static String TEST_TEMPLATE_POP_LOCAL = """
            @SP
            AM=M-1
            D=M
            @R13
            M=D
            @LCL
            D=M
            @R14
            M=D
            @{0}
            D=A
            @LCL
            (POP_LCL.{1}.LOOP)
            D=D-1
            @POP_LCL.{1}.END
            D;JLT
            @R14
            M=M+1
            @POP_LCL.{1}.LOOP
            D;JGE
            (POP_LCL.{1}.END)
            @R13
            D=M
            @R14
            A=M
            M=D""";

    private final static String TEST_TEMPLATE_POP_ARGUMENT = """
            @SP
            AM=M-1
            D=M
            @R13
            M=D
            @ARG
            D=M
            @R14
            M=D
            @{0}
            D=A
            @ARG
            (POP_ARG.{1}.LOOP)
            D=D-1
            @POP_ARG.{1}.END
            D;JLT
            @R14
            M=M+1
            @POP_ARG.{1}.LOOP
            D;JGE
            (POP_ARG.{1}.END)
            @R13
            D=M
            @R14
            A=M
            M=D""";

    private final static String TEST_TEMPLATE_POP_THIS = """
            @SP
            AM=M-1
            D=M
            @R13
            M=D
            @THIS
            D=M
            @R14
            M=D
            @{0}
            D=A
            @THIS
            (POP_THIS.{1}.LOOP)
            D=D-1
            @POP_THIS.{1}.END
            D;JLT
            @R14
            M=M+1
            @POP_THIS.{1}.LOOP
            D;JGE
            (POP_THIS.{1}.END)
            @R13
            D=M
            @R14
            A=M
            M=D""";

    private final static String TEST_TEMPLATE_POP_THAT = """
            @SP
            AM=M-1
            D=M
            @R13
            M=D
            @THAT
            D=M
            @R14
            M=D
            @{0}
            D=A
            @THAT
            (POP_THAT.{1}.LOOP)
            D=D-1
            @POP_THAT.{1}.END
            D;JLT
            @R14
            M=M+1
            @POP_THAT.{1}.LOOP
            D;JGE
            (POP_THAT.{1}.END)
            @R13
            D=M
            @R14
            A=M
            M=D""";

    private final static String TEST_TEMPLATE_POP_STATIC = """
            @SP
            AM=M-1
            D=M
            @R{0}
            M=D""";

    private final static String TEST_TEMPLATE_POP_POINTER = """
            @SP
            AM=M-1
            D=M
            @R{0}
            M=D""";

    private final static Map<String, String> TEST_NO_ARG_TEMPLATE = new HashMap<>();

    static {
        Arrays.stream(TranslationTemplates.INSTANCE.NO_ARG_COMMANDS).forEach(command ->
            TEST_NO_ARG_TEMPLATE.put(command, MessageFormat.format("""
                        function-name = {0}
                        function-args = N/A""", command)));
    }

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

    public String getPushStaticTemplateFor(Object... args) {
        return getTemplateFor(TEST_TEMPLATE_PUSH_STATIC, args);
    }

    public String getPushPointerTemplateFor(Object... args) {
        return getTemplateFor(TEST_TEMPLATE_PUSH_POINTER, args);
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

    public String getPopStaticTemplateFor(Object... args) {
        return getTemplateFor(TEST_TEMPLATE_POP_STATIC, args);
    }

    public String getPopPointerTemplateFor(Object... args) {
        return getTemplateFor(TEST_TEMPLATE_POP_POINTER, args);
    }

    public String getNoArgCommandTemplateFor(String command) {
        return TEST_NO_ARG_TEMPLATE.get(command);
    }

    private String getTemplateFor(String template, Object... args) {
        return MessageFormat.format(template, args);
    }
}
