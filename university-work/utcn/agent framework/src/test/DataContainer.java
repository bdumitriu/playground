package test;

import af.AFCommObject;

/**
 * Created by IntelliJ IDEA.
 * User: tudorm
 * Date: Aug 21, 2003
 * Time: 5:58:01 PM
 * To change this template use Options | File Templates.
 */
public class DataContainer implements AFCommObject
{
	private int arg1;
	private int arg2;
	private int ret;

	public int getArg1()
	{
		return arg1;
	}

	public void setArg1(int arg1)
	{
		this.arg1 = arg1;
	}

	public int getArg2()
	{
		return arg2;
	}

	public void setArg2(int arg2)
	{
		this.arg2 = arg2;
	}

	public int getRet()
	{
		return ret;
	}

	public void setRet(int ret)
	{
		this.ret = ret;
	}
}
