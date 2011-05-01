
import javax.swing.*;
import javax.swing.text.StyledDocument;

/**
 * Created by IntelliJ IDEA.
 * User: bdumitriu
 * Date: Feb 20, 2003
 * Time: 10:02:23 PM
 * To change this template use Options | File Templates.
 */
public class JSpiderTextPane extends JTextPane
{
	public JSpiderTextPane(StyledDocument doc)
	{
		super(doc);
	}

	public boolean getScrollableTracksViewportWidth()
	{
		return false;
	}
}
