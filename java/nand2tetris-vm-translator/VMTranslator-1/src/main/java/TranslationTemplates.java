import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import util.Utils;

import java.io.IOException;
import java.text.MessageFormat;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public enum TranslationTemplates {

    INSTANCE;

    private final Map<String, Map<String, String>> commandMap = new HashMap<>();

    TranslationTemplates() {
        final Map<String, String> pushCommandMap = new HashMap<>();
        putTemplateInMap(pushCommandMap, "constant", "templates/push-constant.asm");
        putTemplateInMap(pushCommandMap, "temp", "templates/push-temp.asm");
        putTemplateInMap(pushCommandMap, "local", "templates/push-segment.asm", "LCL", "{0}");
        putTemplateInMap(pushCommandMap, "argument", "templates/push-segment.asm", "ARG", "{0}");
        putTemplateInMap(pushCommandMap, "this", "templates/push-segment.asm", "THIS", "{0}");
        putTemplateInMap(pushCommandMap, "that", "templates/push-segment.asm", "THAT", "{0}");

        final Map<String, String> popCommandMap = new HashMap<>();
        putTemplateInMap(popCommandMap, "constant", "templates/pop-constant.asm");
        putTemplateInMap(popCommandMap, "temp", "templates/pop-temp.asm");
        putTemplateInMap(popCommandMap, "local", "templates/pop-segment.asm", "LCL", "{0}");
        putTemplateInMap(popCommandMap, "argument", "templates/pop-segment.asm", "ARG", "{0}");
        putTemplateInMap(popCommandMap, "this", "templates/pop-segment.asm", "THIS", "{0}");
        putTemplateInMap(popCommandMap, "that", "templates/pop-segment.asm", "THAT", "{0}");

        commandMap.put("push", pushCommandMap);
        commandMap.put("pop", popCommandMap);
    }

    private void putTemplateInMap(Map<String, String> map, String command, String templateResource, Object... args) {
        final Logger logger = LoggerFactory.getLogger(TranslationTemplates.class);

        try {
            map.put(command, MessageFormat.format(Utils.getStringFromResource(templateResource), args).trim());
        } catch (IOException e) {
            logger.error("Error reading file", e);
            map.put(command, templateResource + " not found");
        }
    }

    public String getTemplate(String command, String argument) {
        return commandMap.getOrDefault(command, Collections.emptyMap()).get(argument);
    }
}
