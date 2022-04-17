import java.text.MessageFormat;

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
        } else if ("push".equals(command)) {
            return translatePushPop(vmCommandParts, command);
        } else if ("pop".equals(command)) {
            return translatePushPop(vmCommandParts, command);
        } else {
            throw new VMSyntaxError("Syntax error: `" + command + "' is an unknown command.");
        }
    }

    private String translatePushPop(String[] vmCommandParts, String command) {
        if (vmCommandParts.length != 3) {
            throw new VMSyntaxError("Syntax error: `" + command + "' expects two arguments.");
        } else {
            final String argument = vmCommandParts[1];
            final String index = vmCommandParts[2];
            final String template = TranslationTemplates.INSTANCE.getTemplate(command, argument);
            if (template == null) {
                throw new VMSyntaxError("Syntax error: `" + argument + "' is an unknown argument for `push'.");
            } else {
                return formatTemplate(template, index);
            }
        }
    }

    private String formatTemplate(String template, Object... args) {
        return MessageFormat.format(template, args);
    }
}
