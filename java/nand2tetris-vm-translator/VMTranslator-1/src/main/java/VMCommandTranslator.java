import java.text.MessageFormat;
import java.util.Arrays;

/**
 * Translates VM commands to ASM commands. See <a href="https://www.nand2tetris.org/">nand2tetris</a> for details.
 */
public class VMCommandTranslator {

    private final ThreadLocal<VMTranslationContext> translationContext = new ThreadLocal<>();

    /**
     * Sets the {@code translationContext} for the current thread.
     */
    public void setTranslationContext(VMTranslationContext translationContext) {
        this.translationContext.set(translationContext);
    }

    public VMTranslationContext getTranslationContext() {
        return translationContext.get();
    }

    /**
     * @return the ASM code to which {@code vmCommandString} translates to. If a {@link VMTranslationContext} wasn't
     * already set, a default one will be set automatically before proceeding.
     */
    public String translate(String vmCommandString) {
        ensureTranslationContext();
        final String[] vmCommandParts = parseCommand(vmCommandString);
        if (noOrBlankCommandParts(vmCommandParts)) {
            return "";
        } else {
            return translate(vmCommandParts);
        }
    }

    private void ensureTranslationContext() {
        if (getTranslationContext() == null) {
            setTranslationContext(new VMTranslationContext());
        }
    }

    private String[] parseCommand(String vmCommandString) {
        return vmCommandString.trim().split("\\h+");
    }

    private boolean noOrBlankCommandParts(String[] vmCommandParts) {
        return vmCommandParts.length == 0 || vmCommandParts[0].isBlank();
    }

    private String translate(String[] vmCommandParts) {
        final String command = vmCommandParts[0];
        if (command.startsWith("//")) {
            return "";
        } else if ("push".equals(command) || "pop".equals(command)) {
            return translatePushPop(command, vmCommandParts);
        } else if (Arrays.asList(VMTranslationTemplates.COMMANDS_WITHOUT_MODIFIERS).contains(command)) {
            return translateArithmeticLogicCommand(vmCommandParts, command);
        } else {
            throw new VMSyntaxException("`" + command + "' is an unknown command.");
        }
    }

    private String translatePushPop(String command, String[] vmCommandParts) {
        if (vmCommandParts.length != 3) {
            throw new VMSyntaxException("`" + command + "' expects two arguments.");
        } else {
            final String modifier = vmCommandParts[1];
            final int index;
            try {
                index = Integer.parseInt(vmCommandParts[2]);
            } catch (NumberFormatException e) {
                throw new VMSyntaxException("'" + vmCommandParts[2] + "' is not a valid integer.");
            }
            return translatePushPop(command, modifier, index);
        }
    }

    private String translatePushPop(String command, String modifier, int index) {
        final VMTranslationTemplates.Template template = VMTranslationTemplates.INSTANCE.getTemplate(command, modifier);
        if (template == null) {
            throw new VMSyntaxException("`" + modifier + "' is an unknown modifier for `" + command + "'.");
        } else if ("temp".equals(modifier)) {
            return translatePushPopTemp(command, index, template);
        } else if ("static".equals(modifier)) {
            return translatePushPopStatic(command, index, template);
        } else if ("pointer".equals(modifier)) {
            return translatePushPopPointer(command, index, template);
        } else {
            return formatTemplate(template, index);
        }
    }

    private String translatePushPopTemp(String command, int index, VMTranslationTemplates.Template template) {
        return translatePushPopRegistry(command, "temp", index, template, 7, 5);
    }

    private String translatePushPopStatic(String command, int index, VMTranslationTemplates.Template template) {
        return translatePushPopRegistry(command, "static", index, template, 239, 16);
    }

    private String translatePushPopPointer(String command, int index, VMTranslationTemplates.Template template) {
        return translatePushPopRegistry(command, "pointer", index, template, 1, 3);
    }

    private String translatePushPopRegistry(
            String command, String modifier, int index, VMTranslationTemplates.Template template, int maxIndex,
            int baseIndex) {
        if (index >= 0 && index <= maxIndex) {
            return formatTemplate(template, baseIndex + index);
        } else {
            throw new VMSyntaxException("'" + index + "' is an invalid index for `" + command + " " + modifier + "'.");
        }
    }

    private String translateArithmeticLogicCommand(String[] vmCommandParts, String command) {
        if (vmCommandParts.length != 1) {
            throw new VMSyntaxException("`" + command + "' expects no arguments.");
        } else {
            final VMTranslationTemplates.Template template = VMTranslationTemplates.INSTANCE.getTemplate(command);
            if (template == null) {
                throw new VMSyntaxException("`" + command + "' is an unknown command.");
            } else {
                return formatTemplate(template);
            }
        }
    }

    private String formatTemplate(VMTranslationTemplates.Template template, Object... args) {
        final Object[] augmentedArgs;
        if (template.needsAutoGeneratedIndex()) {
            augmentedArgs = Arrays.copyOf(args, args.length + 1);
            augmentedArgs[augmentedArgs.length - 1] = getTranslationContext().getNextAutoGeneratedIndex();
        } else {
            augmentedArgs = args;
        }
        return MessageFormat.format(template.template(), augmentedArgs);
    }
}
