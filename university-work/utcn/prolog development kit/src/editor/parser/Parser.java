package editor.parser;

import javax.swing.event.DocumentEvent;
import javax.swing.text.Element;

/**
 * This interface should be implemented by specific parsers in order to
 * make themselves available for use to the PDK document model.
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@bdumitriu.ro
 * @version 0.1
 * 
 * Date: Feb 26, 2003
 */
public interface Parser
{
	/**
	 * This method updates a user-defined tree with <code>rootElement</code>
	 * as its root element after a document update.
	 *
	 * @param event the DocumentEvent containing the change in the document.
	 * @param rootElement the root node of the user-defined tree.
	 */
	public PDKChange update(DocumentEvent event, Element rootElement);
}
