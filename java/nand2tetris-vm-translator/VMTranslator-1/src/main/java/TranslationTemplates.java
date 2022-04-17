import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import util.Utils;

import java.io.IOException;
import java.text.MessageFormat;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public enum TranslationTemplates {

    INSTANCE;

    private final Map<String, Map<String, String>> commandMap = new HashMap<>();

    private static final String NO_ARG_COMMAND_KEY = "no arguments command key";

    public final String[] NO_ARG_COMMANDS = {"add", "sub", "neg", "eq", "lt", "gt", "and", "or", "not"};

    TranslationTemplates() {
        final Map<String, String> pushCommandMap = new HashMap<>();
        putTemplateInMap(pushCommandMap, "constant", "templates/push-constant.asm");
        putTemplateInMap(pushCommandMap, "temp", "templates/push-registry.asm");
        putTemplateInMap(pushCommandMap, "local", "templates/push-segment.asm", "LCL");
        putTemplateInMap(pushCommandMap, "argument", "templates/push-segment.asm", "ARG");
        putTemplateInMap(pushCommandMap, "this", "templates/push-segment.asm", "THIS");
        putTemplateInMap(pushCommandMap, "that", "templates/push-segment.asm", "THAT");
        putTemplateInMap(pushCommandMap, "static", "templates/push-registry.asm");
        putTemplateInMap(pushCommandMap, "pointer", "templates/push-registry.asm");

        final Map<String, String> popCommandMap = new HashMap<>();
        putTemplateInMap(popCommandMap, "temp", "templates/pop-registry.asm");
        putTemplateInMap(popCommandMap, "local", "templates/pop-segment.asm", "LCL");
        putTemplateInMap(popCommandMap, "argument", "templates/pop-segment.asm", "ARG");
        putTemplateInMap(popCommandMap, "this", "templates/pop-segment.asm", "THIS");
        putTemplateInMap(popCommandMap, "that", "templates/pop-segment.asm", "THAT");
        putTemplateInMap(popCommandMap, "static", "templates/pop-registry.asm");
        putTemplateInMap(popCommandMap, "pointer", "templates/pop-registry.asm");

        final Map<String, String> noArgCommandMap = new HashMap<>();
        Arrays.stream(NO_ARG_COMMANDS).forEach(command ->
                putTemplateInMap(noArgCommandMap, command, "templates/" + command + ".asm"));

        commandMap.put("push", pushCommandMap);
        commandMap.put("pop", popCommandMap);
        commandMap.put(NO_ARG_COMMAND_KEY, noArgCommandMap);
    }

    private void putTemplateInMap(Map<String, String> map, String command, String templateResource, Object... args) {
        final Logger logger = LoggerFactory.getLogger(TranslationTemplates.class);

        try {
            final String template;
            if (args.length == 0) {
                template = Utils.getStringFromResource(templateResource);
            } else {
                template = MessageFormat.format(Utils.getStringFromResource(templateResource), args);
            }
            map.put(command, template.trim());
        } catch (IOException e) {
            logger.error("Error reading file `" + templateResource + "'", e);
            map.put(command, templateResource + " not found");
        }
    }

    public String getTemplate(String command) {
        return commandMap.getOrDefault(NO_ARG_COMMAND_KEY, Collections.emptyMap()).get(command);
    }

    public String getTemplate(String command, String modifier) {
        return commandMap.getOrDefault(command, Collections.emptyMap()).get(modifier);
    }
}
