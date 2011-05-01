package geditor.engine.operations;

import geditor.elements.GRootGroup;
import geditor.elements.GShape;

import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: avancea
 * Date: Mar 17, 2004
 * Time: 3:28:54 PM
 * To change this template use File | Settings | File Templates.
 */
public class ScaleOperation
        extends Operation
{
	private double refx;
	private double refy;
	private double tx;
	private double ty;

	private double objrfx = 0;
	private double objrfy = 0;
	private boolean applied = false;

	public ScaleOperation(String shapeId, double refx, double refy, double tx, double ty)
	{
		this.setRefx(refx);
		this.setRefy(refy);
		this.setTx(tx);
		this.setTy(ty);
		this.shapeId = shapeId;
	}

	public ScaleOperation(String shapeId, double refx, double refy, double tx, double ty, double objrfx, double objrfy)
	{
		this.setRefx(refx);
		this.setRefy(refy);
		this.setTx(tx);
		this.setTy(ty);
		this.objrfx = objrfx;
		this.objrfy = objrfy;
		this.applied = true;
		this.shapeId = shapeId;
	}

	public ArrayList invert()
	{
		ArrayList result = new ArrayList();
		ScaleOperation sop = new ScaleOperation(shapeId, getRefx(), getRefy(), 1  / getTx(), 1 / getTy());
		sop.applied = applied;
		sop.objrfx = objrfx;
		sop.objrfy = objrfy;
		result.add(sop);
		return result;
	}


	public double getObjrfx()
	{
		return objrfx;
	}

	public double getObjrfy()
	{
		return objrfy;
	}

	public void applyTo(GRootGroup g)
	{
		GShape shape = g.getShape(shapeId);
		if (shape == null) return;

		if (!applied)
		{
			shape.scale(getRefx(), getRefy(), getTx(), getTy());
			objrfx = shape.getRfx();
			objrfy = shape.getRfy();
			applied = true;
		}
		else
		{
			shape.scale(getRefx() - (objrfx - shape.getRfx()),
			            getRefy() - (objrfy - shape.getRfy()), getTx(), getTy());
		}

	}

	public String toString()
	{
		return "scale " + shapeId + " [" + getRefx() +", " + getRefy() +"] " + getTx() + " " + getTy() +
		        " {" + getObjrfx() +", " + getObjrfy() + " }";

	}

	public double getRefx()
	{
		return refx;
	}

	public void setRefx(double refx)
	{
		this.refx = refx;
	}

	public double getRefy()
	{
		return refy;
	}

	public void setRefy(double refy)
	{
		this.refy = refy;
	}

	public double getTx()
	{
		return tx;
	}

	public void setTx(double tx)
	{
		this.tx = tx;
	}

	public double getTy()
	{
		return ty;
	}

	public void setTy(double ty)
	{
		this.ty = ty;
	}
}
