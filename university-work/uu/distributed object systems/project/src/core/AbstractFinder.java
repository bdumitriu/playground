package core;

import java.util.Map;
import java.util.Hashtable;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Connection;
import java.sql.ResultSet;

/**
 * This class represents a superclass of all finders. It provides some basic functionality which can be used by means
 * of "plugin methods". Also, it provides a identity map implementation by which it makes sure that the same object is
 * not loaded twice. This is done by mapping objects in the map indexed by the id and, when a call for finding an object
 * is made, the object is first looked for into the map and only if it does not exist there is it loaded from the
 * database (and stored in the map as well).
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Mar 9, 2005
 */
public abstract class AbstractFinder
{
	/**
	 * Returns the SQL code which selects a row from a table. Each subclass should implement this appropriately so
	 * that it returns a valid SQL statement for selecting rows from the table the subclass handles.
	 *
	 * @return the SQL code which selects a row from a table.
	 */
	abstract protected String findStatement();

	/**
	 * Does the actual loading of the fields of an object from a database row. This method is particular to each
	 * object, so the subclasses should implement it appropriately. You should expect the result set which
	 * represents the outcome of running the SQL statement defined by {@link #findStatement()}, with its pointer
	 * already set to the row which you have to use.
	 *
	 * @param id the id of this object.
	 * @param rs the result set containing the row to use for setting data.
	 * @return the domain object which was filled with data.
	 */
	abstract protected DomainObject doLoad(String id, ResultSet rs) throws SQLException;

	/**
	 * Provides the basic implementation which can be used by all subclasses for providing find by id functionality.
	 * Subclass have to provide the {@link #findStatement()} and the {@link #doLoad(String, java.sql.ResultSet)}
	 * methods, and then just call {@link #abstractFind(String)} and let it do the work.
	 *
	 * @param id the id of the object to retrieve from the database.
	 * @return the domain object returned by the {@link #doLoad(String, java.sql.ResultSet)} method.
	 */
	synchronized protected DomainObject abstractFind(String id)
	{
		DomainObject result = loadedMap.get(id);
		if (result != null)
		{
			return result;
		}

		Connection conn = ConnectionManager.getInstance().getConnection();
		PreparedStatement findStatement = null;
		try
		{
			findStatement = conn.prepareStatement(findStatement());
			findStatement.setString(1, id);
			ResultSet rs = findStatement.executeQuery();
			if (rs.next())
			{
				result = load(rs);
			}
			else
			{
				result = null;
			}
			conn.close();

			return result;
		}
		catch (SQLException e)
		{
			try
			{
				conn.close();
			}
			catch (SQLException se)
			{
				System.out.println("An SQL exceptions occured: " + se.getMessage());
			}

			System.out.println("An SQL exceptions occured: " + e.getMessage());

			return null;
		}
	}

	/**
	 * Does some administration and then delegates to the {@link #doLoad(String, java.sql.ResultSet)} of the
	 * subclass for doing the actual load.
	 *
	 * @param rs the result set containing the row to use for setting data.
	 * @return the created domain object.
	 */
	synchronized protected DomainObject load(ResultSet rs) throws SQLException
	{
		String id = rs.getString(1);
		if (loadedMap.containsKey(id))
		{
			return loadedMap.get(id);
		}
		DomainObject result = doLoad(id, rs);
		loadedMap.put(id, result);

		return result;
	}

	/**
	 * Adds the specified <code>object</code> to the loaded objects map. You will normally want this when you have
	 * an in-memory object which represents a database row and the object has not been retrieved through the finder.
	 * An example of such a case is when you create a new object and insert it into the database. If an object with
	 * the same key already exists in the map, the program will stop running, since this indicates a problem.
	 *
	 * @param id the id to use as a key in the map.
	 * @param object the object to add to the map.
	 */
	public void addToMap(String id, DomainObject object)
	{
		try
		{
			loadedMap.put(id, object);
		}
		catch (NullPointerException e)
		{}
	}

	/**
	 * Removes the object with the specified <code>id</code> from the loaded objects map. A typical case when you
	 * should do this is when you delete an object from the database. If an object mapped to the given
	 * <code>id</code> doesn't exist in the loaded map, the program will stop running, since this indicates a
	 * problem.
	 *
	 * @param id the id which identifies the entry in the map to remove
	 * @return the removed object.
	 */
	public DomainObject removeFromMap(String id)
	{
		DomainObject removedObj = null;
		try
		{
			removedObj = loadedMap.remove(id);
		}
		catch (NullPointerException e)
		{}

		return removedObj;
	}

	/**
	 * Returns the object bound to the specified <code>id</code>, if such an object exists or null otherwise.
	 *
	 * @param id the id which identifies the entry in the map you want to retrieve.
	 * @return the object bound to the specified <code>id</code>, if such an object exists or null otherwise.
	 */
	public DomainObject getFromMap(String id)
	{
		try
		{
			return loadedMap.get(id);
		}
		catch (NullPointerException e)
		{
			return null;
		}
	}

	protected Map<String, DomainObject> loadedMap = new Hashtable<String, DomainObject>();
}
