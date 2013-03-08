/**
 * @author Bogdan Dumitriu
 */

function finalizeMovieCell(data) {
    if (data.status == "success") {
        addSliders();
    }
}

function configureMovieCellControls() {
    $(".sch_container").on("mouseenter", ".sch_movie_cell", function() {
        $(this).find(".sch_check, .sch_pin_user, .sch_priority_wrapper").each(function() {
            $(this).css({visibility: "visible"});
        });
    }).on("mouseenter", ".sch_priority_wrapper", function() {
        showTooltip($(this).find(".ui-slider-handle").eq(0));
        $(this).css({opacity: "1.0"});
    }).on("mouseleave", ".sch_movie_cell", function() {
        $(this).find(".sch_check, .sch_pin_user, .sch_priority_wrapper").each(function() {
            $(this).css({visibility: "hidden"});
        });
    }).on("mouseleave", ".sch_priority_wrapper", function() {
        $("#tooltip").css({visibility: "hidden"});
        $(this).css({opacity: "0.5"});
    });
}

function removeEmptyHeaderText() {
    $(".sch_header_first, .sch_header_rest").each(function() {
        var maybeEmptyTextNode = $(this).get(0).nextSibling;
        var wsRegexp = new RegExp(/^\s*$/);
        if (maybeEmptyTextNode.nodeType == 3 && wsRegexp.test(maybeEmptyTextNode.nodeValue)) {
            $(maybeEmptyTextNode).remove();
        }
    });
}

function resizeTableBody() {
    var totalWidth = 0;
    $(".sch_header_first, .sch_header_rest").each(function() {
        totalWidth += $(this).outerWidth();
    });
    $(".sch_table_body").width(totalWidth + 4); // adding 4 to avoid problems at all zoom levels
}

function addSliders() {
    var sliderDiv = $(".sch_priority:not(.ui-slider)");
    sliderDiv.slider({
        orientation: "horizontal",
        range: "min",
        min: 0,
        max: 2,
        value: 2,
        start: function(event, ui) {
            setTimeout(function() {showTooltip($(ui.handle))}, 10);
        },
        slide: function(event, ui) {
            setTimeout(function() {showTooltip($(ui.handle))}, 10);
        },
        stop: function(event, ui) {
            $("#sch_form").find("input[id*='priority'][type=hidden]").val(ui.value);
            $(ui.handle).parents(".sch_priority_wrapper").find(".priorityButton").click();
        }
    });
    sliderDiv.each(function() {
        $(this).slider("option", "value", parseInt($(this).next("span").text()));
    });
    $(".sch_priority_wrapper").each(function() {
        $(this).css({visibility: "hidden"});
    });
}

/**
 * @param sliderHandle the jQuery object representing the slider's handle
 */
function showTooltip(sliderHandle) {
    var tooltip = $("#tooltip");
    var offset = sliderHandle.offset();
    tooltip.offset({top: offset.top + 40, left: offset.left - (tooltip.outerWidth() - sliderHandle.outerWidth()) / 2});
    tooltip.css({visibility: "visible"});
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
        return nameAndExtension.name.substring(0, nameAndExtension.name.length - textBeforeExtension.length) + nameAndExtension.extension;
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
