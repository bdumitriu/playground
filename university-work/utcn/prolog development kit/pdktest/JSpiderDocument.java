
import javax.swing.text.*;
import javax.swing.undo.UndoableEdit;
import java.util.Enumeration;
import java.util.Dictionary;
import java.awt.*;
import java.awt.geom.AffineTransform;
import java.awt.font.LineMetrics;
import java.awt.font.FontRenderContext;

/**
 * Created by IntelliJ IDEA.
 * User: bdumitriu
 * Date: Feb 17, 2003
 * Time: 8:12:36 PM
 * To change this template use Options | File Templates.
 */
public class JSpiderDocument extends DefaultStyledDocument
{
	private Style s;			/* syntax highlight style */
	private StyleContext sc;
	private Style docStyle;
	private DefaultHighlighter highlighter;

	public JSpiderDocument()
	{
		super();

		// initialize style
		sc = (StyleContext) getAttributeContext();
		s = sc.addStyle(null, null);
		StyleConstants.setForeground(s, Color.RED);

		// initialize document-wide style
		docStyle = getLogicalStyle(0);
		initStyle(docStyle);
		setParagraphAttributes(0, 0, docStyle, true);
	}

	public void insertString(int offs, String str, AttributeSet a) throws BadLocationException
	{
		super.insertString(offs, str, a);
		highlightSyntax(offs);
		indent(offs, str);
		highlightCurrentLine();
	}

	private void highlightCurrentLine()
	{
		try
		{
			highlighter.addHighlight(0, 100, new DefaultHighlighter.DefaultHighlightPainter(new Color(200, 200, 200, 150)));
		}
		catch (BadLocationException e)
		{
			e.printStackTrace();  //To change body of catch statement use Options | File Templates.
		}
	}

	private void highlightSyntax(int offs)
	{
		try
		{
			if (offs > 4)
			{
				if (getText(offs - 5, 6).equals("append"))
				{
					setCharacterAttributes(offs-5, 6, s, true);
					setCharacterAttributes(offs+1, 0, docStyle, true);
				}
			}
		}
		catch (BadLocationException e)
		{}
	}

	private void indent(int offs, String str)
	{
		char[] ins = str.toCharArray();
		try
		{
			if ((ins[0] == '\n') &&
				((getText(offs - 2, 2).equals(":-")) || (getText(offs - 1, 1).equals(","))))
			{
				super.insertString(offs+1, "\t", null);
			}
		}
		catch (BadLocationException e)
		{}
	}

	private void initStyle(Style style)
	{
		StyleConstants.setFontFamily(style, "Monospaced");
		StyleConstants.setTabSet(style, createTabSet(style));
	}

	private TabSet createTabSet(Style style)
	{
		TabStop[] tStops = new TabStop[1000];
		int sizeOfSpace = 7;	/* size of ' ' character in Monospace */
		int tabSize = 8;

		for (int i = 0; i < 1000; i++)
		{
			tStops[i] = new TabStop(i * sizeOfSpace * tabSize);
		}

		return new TabSet(tStops);
	}

	public void setHighlighter(DefaultHighlighter highlighter)
	{
		this.highlighter = highlighter;
	}
}
