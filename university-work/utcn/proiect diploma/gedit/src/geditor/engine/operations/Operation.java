package geditor.engine.operations;

import geditor.elements.GRootGroup;
import geditor.elements.GShape;

import java.util.ArrayList;
import java.io.Serializable;
import java.net.UnknownHostException;


public abstract class Operation
        implements Cloneable, Serializable

{
	protected String id;

	protected String shapeId;

	protected boolean synched;

	protected ArrayList integratedOps;

	transient boolean fromRepository;


	public Operation()
	{
		//generatates id
		try
		{
			id = java.net.InetAddress.getLocalHost().toString() + (new java.rmi.server.UID()).toString();
			synched = false;
			integratedOps = null;
		}
		catch (UnknownHostException e)
		{
			e.printStackTrace();
		}
	}

	public boolean isFromRepository()
	{
		return fromRepository;
	}

	public void setFromRepository(boolean fromRepository)
	{
		this.fromRepository = fromRepository;
	}

	public String getShapeId()
	{
		return shapeId;
	}

	public String getId()
	{
		return id;
	}

	public abstract ArrayList invert();

	public void setSynched(boolean synched)
	{
		if (this.synched == synched)
		{
			return;
		}

		this.synched = synched;
		if (synched)
		{
			integratedOps = new ArrayList();
			integratedOps.add(this.id);
		}
		else
		{
			integratedOps = null;
		}
	}

	public boolean isSynched()
	{
		return synched;
	}

	public ArrayList getIntegratedOps()
	{
		return integratedOps;
	}

	public void addIntegratedOp(String id)
	{
		synched = true;
		if (integratedOps == null)
		{
			integratedOps = new ArrayList();
			integratedOps.add(this.id);
		}
		integratedOps.add(id);
	}

	public void addIntegratedOps(ArrayList integratedOps)
	{
		synched = true;
		if (this.integratedOps == null)
		{
			this.integratedOps = new ArrayList();
			this.integratedOps.add(this.id);
		}
		if (integratedOps != null)
		{
			this.integratedOps.addAll(integratedOps);
		}
	}

	public  Object clone()
	{
		try
		{
			Operation dolly;
			dolly =  (Operation) super.clone();
			if (integratedOps == null)
			{
				dolly.integratedOps = null;
			}
			else
			{
				dolly.integratedOps = (ArrayList) integratedOps.clone();
			}

			return dolly;
		}
		catch (CloneNotSupportedException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
			return null;
		}
	}


	public void applyTo(GRootGroup g)
	{
		//marking dirty flag
		GShape  shape = g.getShape(shapeId);
		while (shape != null)
		{
			shape.setDirty(true);
			shape = shape.getParent();
		}
	}

	public void setShapeId(String shapeId)
	{
		this.shapeId = shapeId;
	}
}