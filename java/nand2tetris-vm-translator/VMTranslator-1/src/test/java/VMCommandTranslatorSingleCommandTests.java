import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.Arrays;
import java.util.stream.Stream;

public class VMCommandTranslatorSingleCommandTests {

    @DisplayName("Empty lines translate to empty lines")
    @Test
    public void testEmptyLine() {
        final String command = "";
        Assertions.assertEquals("", new VMCommandTranslator().translate(command));
    }

    @DisplayName("Lines with nothing but whitespace translate to empty lines")
    @Test
    public void testWhitespaceLine() {
        final String command = " \t \t\t   ";
        Assertions.assertEquals("", new VMCommandTranslator().translate(command));
    }

    @DisplayName("Comment lines translate to empty lines")
    @Test
    public void testCommentLine() {
        final String command = "// foo bar baz";
        Assertions.assertEquals("", new VMCommandTranslator().translate(command));
    }

    @DisplayName("Comment lines allow leading whitespace")
    @Test
    public void testCommentLineWithWhitespace() {
        final String command = " \t// foo bar baz";
        Assertions.assertEquals("", new VMCommandTranslator().translate(command));
    }

    @DisplayName("'push' with no arguments fails")
    @Test
    public void testPushNoArguments() {
        final String command = "push";
        Assertions.assertThrows(VMSyntaxException.class, () -> new VMCommandTranslator().translate(command));
    }

    @DisplayName("'push ...' without index fails")
    @Test
    public void testPushNoIndex() {
        final String command = "push constant";
        Assertions.assertThrows(VMSyntaxException.class, () -> new VMCommandTranslator().translate(command));
    }

    @DisplayName("'push' with more than 2 arguments fails")
    @Test
    public void testPushTooManyArguments() {
        final String command = "push constant 10 10";
        Assertions.assertThrows(VMSyntaxException.class, () -> new VMCommandTranslator().translate(command));
    }

    @DisplayName("Unknown commands fail")
    @Test
    public void testUnknownCommand() {
        final String command = "what constant 10";
        Assertions.assertThrows(VMSyntaxException.class, () -> new VMCommandTranslator().translate(command));
    }

    @Test
    public void testPushConstant() {
        final String command = "push constant 10";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPushConstantTemplateFor("10"),
                new VMCommandTranslator().translate(command));
    }

    @DisplayName("Random whitespace doesn't affect translation")
    @Test
    public void testPushConstantWithWhitespace() {
        final String command = " \tpush  constant\t\t10 ";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPushConstantTemplateFor("10"),
                new VMCommandTranslator().translate(command));
    }

    @DisplayName("Large index for 'push constant' translates without additional formatting")
    @Test
    public void testPushConstantLargeNumber() {
        final String command = "push constant 9999999";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPushConstantTemplateFor("9999999"),
                new VMCommandTranslator().translate(command));
    }

    @DisplayName("Index 0 translates correctly")
    @Test
    public void testPushConstantNegativeIndex() {
        final String command = "push constant 0";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPushConstantTemplateFor("0"),
                new VMCommandTranslator().translate(command));
    }

    @DisplayName("Negative index translates correctly")
    @Test
    public void testPushConstant0() {
        final String command = "push constant -9999999";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPushConstantTemplateFor("-9999999"),
                new VMCommandTranslator().translate(command));
    }

    @Test
    public void testPushTemp() {
        final String command = "push temp 6";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPushTempTemplateFor("11"),
                new VMCommandTranslator().translate(command));
    }

    @DisplayName("'push temp' fails with negative index")
    @Test
    public void testPushTempNegativeIndex() {
        final String command = "push temp -6";
        Assertions.assertThrows(VMSyntaxException.class, () -> new VMCommandTranslator().translate(command));
    }

    @DisplayName("'push temp' fails with index > 7")
    @Test
    public void testPushTempTooLargeIndex() {
        final String command = "push temp 8";
        Assertions.assertThrows(VMSyntaxException.class, () -> new VMCommandTranslator().translate(command));
    }

    @DisplayName("'push' with unknown first argument fails")
    @Test
    public void testPushWithUnknownFirstArgument() {
        final String command = "push segment 10";
        Assertions.assertThrows(VMSyntaxException.class, () -> new VMCommandTranslator().translate(command));
    }

    @Test
    public void testPushLocal() {
        final String command = "push local 0";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPushLocalTemplateFor("0", "0"),
                new VMCommandTranslator().translate(command));
    }

    @DisplayName("Large index for 'push local' translates without additional formatting")
    @Test
    public void testPushLocalLargeNumber() {
        final String command = "push local 9999999";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPushLocalTemplateFor("9999999", "0"),
                new VMCommandTranslator().translate(command));
    }

    @Test
    public void testPushArgument() {
        final String command = "push argument 1";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPushArgumentTemplateFor("1", "0"),
                new VMCommandTranslator().translate(command));
    }

    @Test
    public void testPushThis() {
        final String command = "push this 6";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPushThisTemplateFor("6", "0"),
                new VMCommandTranslator().translate(command));
    }

    @Test
    public void testPushThat() {
        final String command = "push that 5";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPushThatTemplateFor("5", "0"),
                new VMCommandTranslator().translate(command));
    }

    @Test
    public void testPushStatic() {
        final String command = "push static 3";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPushStaticTemplateFor("19"),
                new VMCommandTranslator().translate(command));
    }

    @DisplayName("'push static' fails with negative index")
    @Test
    public void testPushStaticNegativeIndex() {
        final String command = "push static -3";
        Assertions.assertThrows(VMSyntaxException.class, () -> new VMCommandTranslator().translate(command));
    }

    @DisplayName("'push static' fails with index > 239")
    @Test
    public void testPushStaticTooLargeIndex() {
        final String command = "push static 240";
        Assertions.assertThrows(VMSyntaxException.class, () -> new VMCommandTranslator().translate(command));
    }

    @Test
    public void testPushPointer0() {
        final String command = "push pointer 0";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPushPointerTemplateFor("3"),
                new VMCommandTranslator().translate(command));
    }

    @Test
    public void testPushPointer1() {
        final String command = "push pointer 1";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPushPointerTemplateFor("4"),
                new VMCommandTranslator().translate(command));
    }

    @Test
    public void testPushPointerOtherThan0And1() {
        final String command = "push pointer 2";
        Assertions.assertThrows(VMSyntaxException.class, () -> new VMCommandTranslator().translate(command));
    }

    @DisplayName("'pop' with no arguments fails")
    @Test
    public void testPopNoArguments() {
        final String command = "pop";
        Assertions.assertThrows(VMSyntaxException.class, () -> new VMCommandTranslator().translate(command));
    }

    @DisplayName("'pop ...' without index fails")
    @Test
    public void testPopNoIndex() {
        final String command = "pop temp";
        Assertions.assertThrows(VMSyntaxException.class, () -> new VMCommandTranslator().translate(command));
    }

    @DisplayName("'pop' with more than 2 arguments fails")
    @Test
    public void testPopTooManyArguments() {
        final String command = "pop temp 10 10";
        Assertions.assertThrows(VMSyntaxException.class, () -> new VMCommandTranslator().translate(command));
    }

    @Test
    public void testPopTemp() {
        final String command = "pop temp 0";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPopTempTemplateFor("5"),
                new VMCommandTranslator().translate(command));
    }

    @DisplayName("'pop' with unknown first argument fails")
    @Test
    public void testPopWithUnknownFirstArgument() {
        final String command = "pop segment 10";
        Assertions.assertThrows(VMSyntaxException.class, () -> new VMCommandTranslator().translate(command));
    }

    @Test
    public void testPopLocal() {
        final String command = "pop local 0";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPopLocalTemplateFor("0", "0"),
                new VMCommandTranslator().translate(command));
    }

    @Test
    public void testPopArgument() {
        final String command = "pop argument 1";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPopArgumentTemplateFor("1", "0"),
                new VMCommandTranslator().translate(command));
    }

    @Test
    public void testPopThis() {
        final String command = "pop this 6";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPopThisTemplateFor("6", "0"),
                new VMCommandTranslator().translate(command));
    }

    @Test
    public void testPopThat() {
        final String command = "pop that 5";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPopThatTemplateFor("5", "0"),
                new VMCommandTranslator().translate(command));
    }

    @Test
    public void testPopStatic() {
        final String command = "pop static 8";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPopStaticTemplateFor("24"),
                new VMCommandTranslator().translate(command));
    }

    @Test
    public void testPopPointer0() {
        final String command = "pop pointer 0";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPopPointerTemplateFor("3"),
                new VMCommandTranslator().translate(command));
    }

    @Test
    public void testPopPointer1() {
        final String command = "pop pointer 1";
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPopPointerTemplateFor("4"),
                new VMCommandTranslator().translate(command));
    }

    static Stream<Arguments> noModifierCommandNames() {
        return Arrays.stream(VMTranslationTemplates.INSTANCE.COMMANDS_WITHOUT_MODIFIERS).map(Arguments::of);
    }

    @DisplayName("No-modifier command with arguments fails")
    @ParameterizedTest(name = "{0}")
    @MethodSource("noModifierCommandNames")
    public void testNoModifierCommandWithArguments(String command) {
        Assertions.assertThrows(VMSyntaxException.class, () -> new VMCommandTranslator().translate(command + " 10"));
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("noModifierCommandNames")
    public void testNoModifierCommand(String command) {
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getNoModifierCommandTemplateFor(command, "0"),
                new VMCommandTranslator().translate(command));
    }

    @DisplayName("Large auto-generated index for 'push local' translates without additional formatting")
    @Test
    public void testPushLocalLargeAutoGeneratedIndex() {
        final String command = "push local 5";
        final VMCommandTranslator translator = new VMCommandTranslator();
        translator.setTranslationContext(new VMTranslationContext() {
            @Override
            public int getNextAutoGeneratedIndex() {
                return 9999999;
            }
        });
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPushLocalTemplateFor("5", "9999999"),
                translator.translate(command));
    }

    @DisplayName("Large auto-generated index for 'pop local' translates without additional formatting")
    @Test
    public void testPopLocalLargeAutoGeneratedIndex() {
        final String command = "pop local 5";
        final VMCommandTranslator translator = new VMCommandTranslator();
        translator.setTranslationContext(new VMTranslationContext() {
            @Override
            public int getNextAutoGeneratedIndex() {
                return 9999999;
            }
        });
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getPopLocalTemplateFor("5", "9999999"),
                translator.translate(command));
    }

    @DisplayName("Large auto-generated index for 'eq' translates without additional formatting")
    @Test
    public void testEqLargeAutoGeneratedIndex() {
        final String command = "eq";
        final VMCommandTranslator translator = new VMCommandTranslator();
        translator.setTranslationContext(new VMTranslationContext() {
            @Override
            public int getNextAutoGeneratedIndex() {
                return 9999999;
            }
        });
        Assertions.assertEquals(
                TestTranslationTemplates.INSTANCE.getNoModifierCommandTemplateFor("eq", "9999999"),
                translator.translate(command));
    }
}
