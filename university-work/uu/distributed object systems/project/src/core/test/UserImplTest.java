package core.test;

/**
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Mar 12, 2005
 */

import junit.framework.*;
import core.UserImpl;
import core.UserFinder;
import core.Granularity;
import core.exceptions.*;

public class UserImplTest extends TestCase
{
	protected void setUp() throws Exception
	{
		user = new UserImpl();
	}

	public void testDbOperations() throws Exception
	{
		setUserDetails1();
		user.setLoginName("test user details 1");

		// check that the object can be inserted successfully
		try
		{
			user.dbInsert("test password");
		}
		catch (Exception e)
		{
			Assert.fail();
		}

		// check that the object retrieved from the loaded map is the same as this one
		Assert.assertEquals(UserFinder.getInstance().find("test user details 1"), user);

		setUserDetails2();

		// check that the object can be updated successfully
		try
		{
			user.dbUpdate();
		}
		catch (Exception e)
		{
			Assert.fail();
		}

		// delete the object to keep the database clean
		try
		{
			user.dbDelete();
		}
		catch (Exception e)
		{
			Assert.fail();
		}
	}

	public void testDbChangePassword() throws Exception
	{
		user.setLoginName("test user pass");

		// check that the object can be inserted successfully
		try
		{
			user.dbInsert("test password");
		}
		catch (Exception e)
		{
			Assert.fail();
		}

		// ok, so i'm testing the loaded map as well... got a problem with that?
		Assert.assertEquals(UserFinder.getInstance().findIfPassword(user.getLoginName(), "test password"), user);

		// check that the password can be changed successfully using dbChangePassword with one argument
		try
		{
			user.dbChangePassword("password2");
		}
		catch (Exception e)
		{
			Assert.fail();
		}

		// check that the object can be retrieved using the new password
		Assert.assertEquals(UserFinder.getInstance().findIfPassword(user.getLoginName(), "password2"), user);

		// check that the password can be changed successfully using dbChangePassword with two arguments
		try
		{
			user.dbChangePassword("password2", "password3");
		}
		catch (Exception e)
		{
			Assert.fail();
		}

		// check that the object can be retrieved using the new password
		Assert.assertEquals(UserFinder.getInstance().findIfPassword(user.getLoginName(), "password3"), user);

		// delete the object to keep the database clean
		try
		{
			user.dbDelete();
		}
		catch (Exception e)
		{
			Assert.fail();
		}
	}

	public void testDbManageLoadedMap() throws Exception
	{
		user.setLoginName("test user");

		// check that the object can be inserted successfully
		try
		{
			user.dbInsert("test password");
		}
		catch (Exception e)
		{
			Assert.fail();
		}

		// check that the object was put in the loaded objects map
		Assert.assertEquals(user, UserFinder.getInstance().find("test user"));

		// check that the object can be deleted successfully
		try
		{
			user.dbDelete();
		}
		catch (Exception e)
		{
			Assert.fail();
		}

		// check that the object was removed from the loaded objects map
		Assert.assertNull(UserFinder.getInstance().find("test user"));

		// check that the object was deleted from the database by trying to insert it again
		// (if it has not been deleted, that the insertion will fail due to duplicat keys)
		try
		{
			user.dbInsert("test password");
		}
		catch (Exception e)
		{
			Assert.fail();
		}

		// check that the object was (once again) put in the loaded objects map
		Assert.assertEquals(user, UserFinder.getInstance().find("test user"));

		// delete the object again to keep the database clean
		try
		{
			user.dbDelete();
		}
		catch (Exception e)
		{
			Assert.fail();
		}

		// check that the object was (once again) removed from the loaded objects map
		Assert.assertNull(UserFinder.getInstance().find("test user"));
	}

	public void testDbInsertExceptions() throws Exception
	{
		user.setLoginName("bdumitriu");

		// check that a user with the same name is not inserted twice
		try
		{
			user.dbInsert("");
			Assert.fail();
		}
		catch (DuplicateLoginNameException e)
		{}
	}

	public void testDbChangePasswordExceptions() throws Exception
	{
		user.setLoginName("frejgkl erj ger gieo wgi rjeoig");

		// check that changing the password for an invalid user using dbChangePassword with one argument
		// results in the expected error code
		try
		{
			user.dbChangePassword("password2");
			Assert.fail();
		}
		catch (InvalidLoginNameException e)
		{}

		// check that changing the password for an invalid user using dbChangePassword with two arguments
		// results in the expected error code
		try
		{
			user.dbChangePassword("password2", "bla bla");
			Assert.fail();
		}
		catch (InvalidLoginNameException e)
		{}

		user.setLoginName("test user pass 2");

		// check that the object can be inserted successfully
		try
		{
			user.dbInsert("test password");
		}
		catch (Exception e)
		{
			Assert.fail();
		}

		// check that trying to change the password without knowing the old password using dbChangePassword
		// with two arguments results in the expected error code
		try
		{
			user.dbChangePassword("password2", "bla bla");
			Assert.fail();
		}
		catch (WrongPasswordException e)
		{}

		// check that the user cannot be retrieved with the new password which has not successfully replaced
		// the old one in the previous step
		Assert.assertNull(UserFinder.getInstance().findIfPassword(user.getLoginName(), "bla bla"));

		// delete the object to keep the database clean
		try
		{
			user.dbDelete();
		}
		catch (Exception e)
		{
			Assert.fail();
		}
	}

	public void testDbUpdateExceptions() throws Exception
	{
		user.setLoginName("ferjgr gerjgrlewig eorgh ouieg");

		// check that trying to update the data for a user using an invalid login name results in the expected
		// error code
		try
		{
			user.dbUpdate();
			Assert.fail();
		}
		catch (InvalidLoginNameException e)
		{}
	}

	public void testDbDeleteExceptions() throws Exception
	{
		user.setLoginName("ferjgr gerjgrlewig eorgh ouieg");

		// check that trying to delete a user using an invalid login name results in the expected error code
		try
		{
			user.dbDelete();
			Assert.fail();
		}
		catch (InvalidLoginNameException e)
		{}
	}

	private void setUserDetails1()
	{
		try
		{
			user.setName("test name 1", false);
			user.setTitle("test title 1", false);
			user.setPhoneNumber("test phone 1", false);
			user.setGranularity(Granularity.FIFTEEN_MINUTES, false);
		}
		catch (InvalidLoginNameException e)
		{
			// exception is never thrown since the persistent argument is false
		}
	}

	private void setUserDetails2()
	{
		try
		{
			user.setName("test name 2", false);
			user.setTitle("test title 2", false);
			user.setPhoneNumber("test phone 2", false);
			user.setGranularity(Granularity.ONE_HOUR, false);
		}
		catch (InvalidLoginNameException e)
		{
			// exception is never thrown since the persistent argument is false
		}
	}

	public static Test suite()
	{
		return new TestSuite(UserImplTest.class);
	}

	UserImpl user;
}