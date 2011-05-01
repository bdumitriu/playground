package geditor.engine.operations;

import geditor.elements.*;

import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: avancea
 * Date: Mar 17, 2004
 * Time: 4:32:45 PM
 * To change this template use File | Settings | File Templates.
 */
public class SetTextOperation
        extends Operation
{

	private String text;
	private String oldtext;

	public SetTextOperation(String shapeId, String text)
	{
		this.shapeId = shapeId;
		this.text = text;
		this.oldtext = null;
	}

	public ArrayList invert()
	{
		ArrayList result = new ArrayList();
		SetTextOperation sto = new SetTextOperation(shapeId, oldtext);
		sto.oldtext = text;
		result.add(sto);
		return result;
	}

	public String getText()
	{
		return text;
	}

	public void applyTo(GRootGroup g)
	{
		GShape shape = g.getShape(shapeId);
		if (shape == null) return;
		if (shape instanceof GText)
		{
			GText stext = (GText)shape;
			oldtext = stext.getText();
			stext.setText(text);
		}
	}

	public String toString()
	{
		return "SetText" + " " + shapeId + " " + text;
	}

}
