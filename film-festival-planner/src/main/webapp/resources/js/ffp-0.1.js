/**
 * @author Bogdan Dumitriu
 */

function configure_mouse_listeners(elem) {
    var overSrc = get_toggled_src(elem, "_mouse_over");
    var outSrc = elem.attr("src");
    elem
        .mouseover(function() {
            $(this).attr("src", overSrc);
        })
        .mouseout(function() {
            $(this).attr("src", outSrc);
        });
}

/**
 * Reads the src attribute of <code>elem</code> and returns "foo_suffix.ext" for src="foo.ext" and vice-versa.
 *
 * @param elem
 *            a jQuery element with a src attribute
 * @param textBeforeExtension
 *            the text to toggle on/off (expected at the end of the name part of the src attribute's name.extension)
 * @return the src attribute with <code>textBeforeExtension</code> toggled on/off.
 */
function get_toggled_src(elem, textBeforeExtension) {
    var currentSrc = elem.attr("src");
    var nameAndExtension = get_name_and_extension(currentSrc);
    return get_toggled_file_name(nameAndExtension, textBeforeExtension);
}

function reconfigure_mouse_listeners(data) {
    if (data.status =="success") {
        $(data.source).parent().find(".sch_movie_button").each(function() {
            configure_mouse_listeners($(this));
        });
    }
}

/**
 * Returns "foo_suffix.ext" for input "foo.ext" and vice-versa.
 *
 * @param nameAndExtension
 *            a map where <code>.name</code> indicates the file name and <code>.extension</code> indicates the file
 *            extension (including the '.' prefix)
 * @param textBeforeExtension
 *            the text to toggle on/off (expected at the end of the name from the <code>nameAndExtension</code>)
 * @return the reconstructed file name with <code>textBeforeExtension</code> toggled on/off.
 */
function get_toggled_file_name(nameAndExtension, textBeforeExtension) {
    if (endsWith(nameAndExtension.name, textBeforeExtension)) {
        return nameAndExtension.name.substring(0, nameAndExtension.name.length - textBeforeExtension.length)
            + nameAndExtension.extension;
    } else {
        return nameAndExtension.name + textBeforeExtension + nameAndExtension.extension;
    }
}

function get_name_and_extension(fileName) {
    var dotIndex = fileName.lastIndexOf(".");
    var name, extension;
    if (dotIndex != -1) {
        name = fileName.substring(0, dotIndex);
        extension = fileName.substring(dotIndex);
    } else {
        name = fileName;
        extension = "";
    }
    return {
        'name': name,
        'extension': extension
    };
}

function endsWith(string, suffix) {
    return string.indexOf(suffix, string.length - suffix.length) !== -1;
}

function writeObj(obj, message) {
    if (!message) { message = obj; }
    var details = "*****************" + "\n" + message + "\n";
    var fieldContents;
    for (var field in obj) {
        fieldContents = obj[field];
        if (typeof(fieldContents) == "function") {
            fieldContents = "(function)";
        }
        details += "  " + field + ": " + fieldContents + "\n";
    }
    console.log(details);
}
