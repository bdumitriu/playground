import java.text.MessageFormat;
import java.util.Arrays;

public class Translator {

    public String translate(String vmCommandString) {
        final String[] vmCommandParts = parseCommand(vmCommandString);
        if (noOrBlankCommandParts(vmCommandParts)) {
            return "";
        } else {
            return translate(vmCommandParts);
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
        if ("//".equals(command)) {
            return "";
        } else if ("push".equals(command) || "pop".equals(command)) {
            return translatePushPop(command, vmCommandParts);
        } else if (Arrays.asList(TranslationTemplates.INSTANCE.NO_ARG_COMMANDS).contains(command)) {
            return translateAddSub(vmCommandParts, command);
        } else {
            throw new VMSyntaxError("`" + command + "' is an unknown command.");
        }
    }

    private String translatePushPop(String command, String[] vmCommandParts) {
        if (vmCommandParts.length != 3) {
            throw new VMSyntaxError("`" + command + "' expects two arguments.");
        } else {
            final String modifier = vmCommandParts[1];
            final int index;
            try {
                index = Integer.parseInt(vmCommandParts[2]);
            } catch (NumberFormatException e) {
                throw new VMSyntaxError("'" + vmCommandParts[2] + "' is not a valid integer.");
            }
            return translatePushPop(command, modifier, index);
        }
    }

    private String translatePushPop(String command, String modifier, int index) {
        final String template = TranslationTemplates.INSTANCE.getTemplate(command, modifier);
        if (template == null) {
            throw new VMSyntaxError("`" + modifier + "' is an unknown modifier for `" + command + "'.");
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

    private String translatePushPopTemp(String command, int index, String template) {
        return translatePushPopRegistry(command, "temp", index, template, 7, 5);
    }

    private String translatePushPopStatic(String command, int index, String template) {
        return translatePushPopRegistry(command, "static", index, template, 239, 16);
    }

    private String translatePushPopPointer(String command, int index, String template) {
        return translatePushPopRegistry(command, "pointer", index, template, 1, 3);
    }

    private String translatePushPopRegistry(
            String command, String modifier, int index, String template, int maxIndex, int baseIndex) {
        if (index >= 0 && index <= maxIndex) {
            return formatTemplate(template, baseIndex + index);
        } else {
            throw new VMSyntaxError("'" + index + "' is an invalid index for `" + command + " " + modifier + "'.");
        }
    }

    private String translateAddSub(String[] vmCommandParts, String command) {
        if (vmCommandParts.length != 1) {
            throw new VMSyntaxError("`" + command + "' expects no arguments.");
        } else {
            final String template = TranslationTemplates.INSTANCE.getTemplate(command);
            if (template == null) {
                throw new VMSyntaxError("`" + command + "' is an unknown command.");
            } else {
                return formatTemplate(template);
            }
        }
    }

    private String formatTemplate(String template, Object... args) {
        return MessageFormat.format(template, args);
    }
}
