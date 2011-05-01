package geditor.engine.operations;

import geditor.elements.GRootGroup;

import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: avancea
 * Date: Mar 17, 2004
 * Time: 2:25:08 PM
 * To change this template use File | Settings | File Templates.
 */

//does nothing
public class NopOperation   extends Operation
{
	public NopOperation()
	{
		shapeId = "";
	}

	public ArrayList invert()
	{
		ArrayList result = new ArrayList();
		result.add((Operation) clone());
		return result;
	}


	public void applyTo(GRootGroup g)
	{
		return;
	}

	public String toString()
	{
		return "nop";
	}

}
