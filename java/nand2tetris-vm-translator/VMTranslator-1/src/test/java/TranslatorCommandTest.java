import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

public class TranslatorCommandTest {

    @DisplayName("Empty lines translate to empty lines")
    @Test
    public void testEmptyLine() {
        final String command = "";
        Assertions.assertEquals("", new Translator().translate(command));
    }

    @DisplayName("Lines with nothing but whitespace translate to empty lines")
    @Test
    public void testWhitespaceLine() {
        final String command = " \t \t\t   ";
        Assertions.assertEquals("", new Translator().translate(command));
    }

    @DisplayName("Comment lines translate to empty lines")
    @Test
    public void testCommentLine() {
        final String command = "// foo bar baz";
        Assertions.assertEquals("", new Translator().translate(command));
    }

    @DisplayName("Comment lines allow leading whitespace")
    @Test
    public void testCommentLineWithWhitespace() {
        final String command = " \t// foo bar baz";
        Assertions.assertEquals("", new Translator().translate(command));
    }

    @DisplayName("'push' with no arguments fails")
    @Test
    public void testPushNoArguments() {
        final String command = "push";
        Assertions.assertThrows(VMSyntaxError.class, () -> new Translator().translate(command));
    }

    @DisplayName("'push constant' without index fails")
    @Test
    public void testPushConstantNoIndex() {
        final String command = "push constant";
        Assertions.assertThrows(VMSyntaxError.class, () -> new Translator().translate(command));
    }

    @DisplayName("'push' with more than 2 arguments fails")
    @Test
    public void testPushTooManyArguments() {
        final String command = "push constant 10 10";
        Assertions.assertThrows(VMSyntaxError.class, () -> new Translator().translate(command));
    }

    @DisplayName("Unknown commands fail")
    @Test
    public void testUnknownCommand() {
        final String command = "what constant 10";
        Assertions.assertThrows(VMSyntaxError.class, () -> new Translator().translate(command));
    }

    @Test
    public void testPushConstant() {
        final String command = "push constant 10";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPushConstantTemplateFor(10),
                new Translator().translate(command));
    }

    @DisplayName("Random whitespace doesn't affect translation")
    @Test
    public void testPushConstantWithWhitespace() {
        final String command = " \tpush  constant\t\t10 ";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPushConstantTemplateFor(10),
                new Translator().translate(command));
    }

    @Test
    public void testPushTemp() {
        final String command = "push temp 6";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPushTempTemplateFor(6),
                new Translator().translate(command));
    }

    @DisplayName("'push' with unknown first argument fails")
    @Test
    public void testPushWithUnknownFirstArgument() {
        final String command = "push segment 10";
        Assertions.assertThrows(VMSyntaxError.class, () -> new Translator().translate(command));
    }

    @Test
    public void testPushLocal() {
        final String command = "push local 0";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPushLocalTemplateFor(0),
                new Translator().translate(command));
    }

    @Test
    public void testPushArgument() {
        final String command = "push argument 1";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPushArgumentTemplateFor(1),
                new Translator().translate(command));
    }

    @Test
    public void testPushThis() {
        final String command = "push this 6";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPushThisTemplateFor(6),
                new Translator().translate(command));
    }

    @Test
    public void testPushThat() {
        final String command = "push that 5";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPushThatTemplateFor(5),
                new Translator().translate(command));
    }

    @DisplayName("'pop' with no arguments fails")
    @Test
    public void testPopNoArguments() {
        final String command = "pop";
        Assertions.assertThrows(VMSyntaxError.class, () -> new Translator().translate(command));
    }

    @DisplayName("'pop constant' without index fails")
    @Test
    public void testPopConstantNoIndex() {
        final String command = "pop constant";
        Assertions.assertThrows(VMSyntaxError.class, () -> new Translator().translate(command));
    }

    @DisplayName("'pop' with more than 2 arguments fails")
    @Test
    public void testPopTooManyArguments() {
        final String command = "pop constant 10 10";
        Assertions.assertThrows(VMSyntaxError.class, () -> new Translator().translate(command));
    }

    @Test
    public void testPopConstant() {
        final String command = "pop constant 10";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPopConstantTemplateFor(10),
                new Translator().translate(command));
    }

    @Test
    public void testPopTemp() {
        final String command = "pop temp 6";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPopTempTemplateFor(6),
                new Translator().translate(command));
    }

    @DisplayName("'pop' with unknown first argument fails")
    @Test
    public void testPopWithUnknownFirstArgument() {
        final String command = "pop segment 10";
        Assertions.assertThrows(VMSyntaxError.class, () -> new Translator().translate(command));
    }

    @Test
    public void testPopLocal() {
        final String command = "pop local 0";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPopLocalTemplateFor(0),
                new Translator().translate(command));
    }

    @Test
    public void testPopArgument() {
        final String command = "pop argument 1";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPopArgumentTemplateFor(1),
                new Translator().translate(command));
    }

    @Test
    public void testPopThis() {
        final String command = "pop this 6";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPopThisTemplateFor(6),
                new Translator().translate(command));
    }

    @Test
    public void testPopThat() {
        final String command = "pop that 5";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPopThatTemplateFor(5),
                new Translator().translate(command));
    }
}
