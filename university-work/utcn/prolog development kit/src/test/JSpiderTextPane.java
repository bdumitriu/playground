package test;

import editor.PDKRootElement;
import editor.PDKToken;
import editor.Constants;

import javax.swing.*;
import javax.swing.event.DocumentListener;
import javax.swing.event.DocumentEvent;
import javax.swing.text.StyledDocument;
import javax.swing.text.BadLocationException;

/**
 * Extension of JTextPane which turns off wrapping.
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@bdumitriu.ro
 * @version 0.1
 *
 * Date: Feb 26, 2003
 */
public class JSpiderTextPane extends JTextPane implements DocumentListener
{
	public JSpiderTextPane(StyledDocument doc)
	{
		super(doc);
		doc.addDocumentListener(this);
	}

	public boolean getScrollableTracksViewportWidth()
	{
		return false;
	}

	public void insertUpdate(DocumentEvent e)
	{
		String str = null;
		try
		{
			int offset = e.getOffset();
			str = e.getDocument().getText(offset, 1);

			if (str.equals(")"))
			{
				PDKRootElement root = ((JSpiderDocument) getDocument()).getRoot();
				int idx = root.getElementIndex(offset);
				PDKToken tok = (PDKToken) root.getElement(idx);

				int matchCount = 0;

				for (int i = idx-1; i >= 0;  i--)
				{
					tok = (PDKToken) root.getElement(i);
					if (tok.getType() == Constants.OPEN_ROUND_BRACKET_ELEMENT_TYPE)
					{
						if (matchCount == 0)
						{
							int pos = getCaret().getDot();

							getCaret().setDot(tok.getStartOffset()+1);
							getCaret().moveDot(tok.getStartOffset());

							getCaret().setDot(pos);
							break;
						}
						else
						{
							matchCount --;
						}
					}
					else if (tok.getType() == Constants.CLOSED_ROUND_BRACKET_ELEMENT_TYPE)
					{
						matchCount ++;
					}
				}

			}
		}
		catch (BadLocationException e1)
		{
			e1.printStackTrace();
		}
	}

	public void removeUpdate(DocumentEvent e)
	{
	}

	public void changedUpdate(DocumentEvent e)
	{
	}
}