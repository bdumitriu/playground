import java.text.MessageFormat;
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

    private final static Map<String, String> TEST_TEMPLATE_COMMANDS_WITHOUT_MODIFIER = new HashMap<>();

    static {
        TEST_TEMPLATE_COMMANDS_WITHOUT_MODIFIER.put("add", """
                @SP
                AM=M-1
                D=M
                @SP
                AM=M-1
                M=M+D
                @SP
                M=M+1""");
        TEST_TEMPLATE_COMMANDS_WITHOUT_MODIFIER.put("sub", """
                @SP
                AM=M-1
                D=M
                @SP
                AM=M-1
                M=M-D
                @SP
                M=M+1""");
        TEST_TEMPLATE_COMMANDS_WITHOUT_MODIFIER.put("eq", """
                @SP
                AM=M-1
                D=M
                @SP
                AM=M-1
                D=M-D
                @COMP.{0}.TRUE
                D;JEQ
                @COMP.{0}.FALSE
                0;JMP
                (COMP.{0}.TRUE)
                @SP
                A=M
                M=-1
                @SP
                M=M+1
                @COMP.{0}.END
                0;JMP
                (COMP.{0}.FALSE)
                @SP
                A=M
                M=0
                @SP
                M=M+1
                (COMP.{0}.END)""");
        TEST_TEMPLATE_COMMANDS_WITHOUT_MODIFIER.put("lt", """
                @SP
                AM=M-1
                D=M
                @SP
                AM=M-1
                D=M-D
                @COMP.{0}.TRUE
                D;JLT
                @COMP.{0}.FALSE
                0;JMP
                (COMP.{0}.TRUE)
                @SP
                A=M
                M=-1
                @SP
                M=M+1
                @COMP.{0}.END
                0;JMP
                (COMP.{0}.FALSE)
                @SP
                A=M
                M=0
                @SP
                M=M+1
                (COMP.{0}.END)""");
        TEST_TEMPLATE_COMMANDS_WITHOUT_MODIFIER.put("gt", """
                @SP
                AM=M-1
                D=M
                @SP
                AM=M-1
                D=M-D
                @COMP.{0}.TRUE
                D;JGT
                @COMP.{0}.FALSE
                0;JMP
                (COMP.{0}.TRUE)
                @SP
                A=M
                M=-1
                @SP
                M=M+1
                @COMP.{0}.END
                0;JMP
                (COMP.{0}.FALSE)
                @SP
                A=M
                M=0
                @SP
                M=M+1
                (COMP.{0}.END)""");
        TEST_TEMPLATE_COMMANDS_WITHOUT_MODIFIER.put("neg", """
                @SP
                AM=M-1
                M=-M
                @SP
                M=M+1""");
        TEST_TEMPLATE_COMMANDS_WITHOUT_MODIFIER.put("and", """
                @SP
                AM=M-1
                D=M
                @SP
                AM=M-1
                M=M&D
                @SP
                M=M+1""");
        TEST_TEMPLATE_COMMANDS_WITHOUT_MODIFIER.put("or", """
                @SP
                AM=M-1
                D=M
                @SP
                AM=M-1
                M=M|D
                @SP
                M=M+1""");
        TEST_TEMPLATE_COMMANDS_WITHOUT_MODIFIER.put("not", """
                @SP
                AM=M-1
                M=!M
                @SP
                M=M+1""");
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

    public String getNoModifierCommandTemplateFor(String command, Object... args) {
        return getTemplateFor(TEST_TEMPLATE_COMMANDS_WITHOUT_MODIFIER.get(command), args);
    }

    private String getTemplateFor(String template, Object... args) {
        return MessageFormat.format(template, args);
    }
}
