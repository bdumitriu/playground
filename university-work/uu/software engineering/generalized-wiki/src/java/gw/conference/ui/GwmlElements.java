package gw.conference.ui;

import java.util.LinkedHashMap;
import java.util.Map;
import gw.GwConstants;

public class GwmlElements {
    // NOTE: This should probably be moved to seperate files,
    // but time constraints are killing us...
	
	/*
	 * Tables
	 */
	public static String table(Map<String, String> map) {
		
		String table = new String("<table>");
		
		for(String name : map.keySet()) {
			table += tableRow(
					 tableCell(name)
				   + tableCell(map.get(name))
				   );
		}

		table += "</table>";
		
		return table;
	}
    public static String tableInTable(Map<String, Map<String, String>> map) {
        
        String table = new String("<table>");

        for(String subTable : map.keySet()) {
            table += tableRow(
                     tableCell(subTable)
                   + tableCell( table(map.get(subTable))
                   ));
        }
        
        table += "</table>";
        return table;

      }
	
	public static String tableCell(String content) {
		return "<col>" + content + "</col>";
	}
	
	public static String tableRow(String content) {
		return "<row>" + content + "</row>";
	}
    
    // TODO: Don't generate HTML here, only GWML...
    
	public static String tableHtml(Map<String, String> map) {
        
        String table = new String("<table>");
        
        for(String name : map.keySet()) {
            table += tableRowHtml(
                     tableCellHtml(name)
                   + tableCellHtml(map.get(name))
                   );
        }

        table += "</table>";
        
        return table;
    }
    
    public static String tableCellHtml(String content) {
        return "<td>" + content + "</td>";
    }
    
    public static String tableRowHtml(String content) {
        return "<tr>" + content + "</tr>";
    }
	
	/*
	 * Sections
	 */
    
    public static String getSection(String title, String body) {
        return "<section><title>" + title + "</title>\n<para>" + body + "</para></section>\n";
    }

    public static String getParagraph(String paragraph) {
        return "<para>"+paragraph+"</para>";
    }
    
	public static String getSingleParagraphPage(String title, String content) {
		return  GwConstants.GWML_ROOT_OPEN_TAG
                + getSection(title, content)
                + GwConstants.GWML_ROOT_CLOSE_TAG;
	}
    
    public static String getPage(String title, String content) {
        return  GwConstants.GWML_ROOT_OPEN_TAG
                + "<section><title>" + title + "</title>\n"
                + content
                + "</section>"
                + GwConstants.GWML_ROOT_CLOSE_TAG;
    }
	
	
	public static String makeLink(String name, String path) {
        return "<link target=\""+path+"\">" + name + "</link>";
	}
	
	/*
	 * Form elements
	 */
    
    /** Create a input form from a map. */
    public static String getFormNiceHtml(Map<String, String> fields, String action, String buttonName ) {
        Map<String, String> toTable = new LinkedHashMap<String, String>();
        
        for (String name : fields.keySet()) {
            toTable.put(name, getTextField(fields.get(name).toString() ));         
        }
        toTable.put("", getButton("submit", buttonName));
        
        
        return getForm(action, tableHtml(toTable));
    }
    
	public static String getTextField(String name, int length) {
		return"<input type=\"text\" maxlength=\"" + length + "\" name=\"" + name +
				"\" />";
	}
	
	public static String getTextField(String name) {
		return "<input type=\"text\" name=\"" + name +
				"\" />";
	}
	
	public static String getTextArea(String name) {
		return "" +
				"<textarea name=\"" + name +"\"></textarea>";
	}
   
	public static String getForm(String action, String content) {
		return "" +
				"<form method=\"post\" action=\"" + action +
				"\">" + content +
				"</form>";
		
	}
	
	public static String getButton(String type, String name) {		
		return "<input type=\"" + type + "\" value=\"" + name + "\" />";
	}
	
	public static String getComponentInParagraph(String componentName, String component) {
        return getParagraph(componentName + component);
	}
	
	public static String getTextFieldInParagraph(String name, String property) {
		return getComponentInParagraph(name, getTextField(property));		
	}
    public static String getTextFieldInParagraph(String property) {
        return getTextFieldInParagraph(property, property);
        
    }
	
	public static String getTextAreaInParagraph(String name) {
		return getComponentInParagraph(name, getTextArea(name));
	}
	

}
