package core.test;

/**
 *  
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Mar 10, 2005
 */

import junit.framework.*;
import core.UserFinder;
import core.UserImpl;
import core.Granularity;

public class UserFinderTest extends TestCase
{
	protected void setUp() throws Exception
	{
		userFinder = UserFinder.getInstance();
	}

	public void testFind() throws Exception
	{
		UserImpl user = userFinder.find("bdumitriu");
		Assert.assertEquals(user.getLoginName(), "bdumitriu");
		Assert.assertEquals(user.getName(), "Bogdan Dumitriu");
		Assert.assertEquals(user.getTitle(), "eng.");
		Assert.assertEquals(user.getPhoneNumber(), "030-2641620");
		Assert.assertEquals(user.getGranularity(), Granularity.THIRTY_MINUTES);
	}

	public void testNoFind() throws Exception
	{
		UserImpl user = userFinder.find("ferkgjregl erhglheuigerge rwgieegweg");
		Assert.assertEquals(user, null);
	}

	public void testLoadedFind() throws Exception
	{
		UserImpl firstUser = userFinder.find("bdumitriu");
		UserImpl secondUser = userFinder.find("bdumitriu");
		Assert.assertEquals(firstUser, secondUser);
	}

	public void testFindIfPassword() throws Exception
	{
		UserImpl user = userFinder.findIfPassword("bdumitriu", "bdumitriu's pass");
		Assert.assertEquals(user.getLoginName(), "bdumitriu");
		Assert.assertEquals(user.getName(), "Bogdan Dumitriu");
		Assert.assertEquals(user.getTitle(), "eng.");
		Assert.assertEquals(user.getPhoneNumber(), "030-2641620");
		Assert.assertEquals(user.getGranularity(), Granularity.THIRTY_MINUTES);
	}

	public void testNoFindIfPassword() throws Exception
	{
		UserImpl user = userFinder.findIfPassword("bdumitriu", "jrkgelrger");
		Assert.assertEquals(user, null);
	}

	public void testLoadedFindIfPassword() throws Exception
	{
		UserImpl firstUser = userFinder.find("bdumitriu");
		UserImpl secondUser = userFinder.findIfPassword("bdumitriu", "bdumitriu's pass");
		Assert.assertEquals(firstUser, secondUser);
	}

	public void testNoLoadedFindIfPassword() throws Exception
	{
		userFinder.find("bdumitriu");
		UserImpl user = userFinder.findIfPassword("bdumitriu", "jrkgelrger");
		Assert.assertEquals(user, null);
	}

	public void testReverseLoadedFindIfPassword() throws Exception
	{
		UserImpl firstUser = userFinder.findIfPassword("bdumitriu", "bdumitriu's pass");
		UserImpl secondUser = userFinder.find("bdumitriu");
		Assert.assertEquals(firstUser, secondUser);
	}

	public static Test suite()
	{
		return new TestSuite(UserFinderTest.class);
	}

	UserFinder userFinder;
}