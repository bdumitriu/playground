package gw.storage.tests;

import java.io.IOException;
import java.util.*;

import org.tmatesoft.svn.core.SVNException;
import org.tmatesoft.svn.core.SVNURL;
import org.tmatesoft.svn.core.internal.io.dav.DAVRepositoryFactory;
import org.tmatesoft.svn.core.internal.io.svn.SVNRepositoryFactoryImpl;
import org.tmatesoft.svn.core.io.SVNRepository;
import org.tmatesoft.svn.core.io.SVNRepositoryFactory;

import gw.storage.StorageException;
import gw.storage.StorageStatusMessage;
import gw.storage.javasvn.*;
import junit.framework.Assert;
import junit.framework.TestCase;

public class PropertyTest extends TestCase {
	
	private SVNRepository _repository;
    private CommitItemContainer _container;
    private StorageStatusMessage _ssm;
    private SVNRAStorage _property;
    
	protected void setUp() throws Exception
    {
            // Set up the repository
            DAVRepositoryFactory.setup();
            SVNRepositoryFactoryImpl.setup();

            String url="svn://localhost:10000";
            String name="gwuser";
            String password="gwpassword";
      
            
            _property = new SVNRAStorage(url, name, password);
            
            
            
    }

	//I try to delete property of a file that doesn't have any properties
	
	public void testDeleteDeletedProperty()
	{
		Map<String, String> properties = new HashMap<String, String>();
		
		try
		{
			_property.setProperty("branches/properties/file", "jgkrlejglkerj", null, false);
			
			_property.commit("...");

			properties = _property.getProperties("branches/properties/file");
			
			assertFalse(properties.containsKey("jgkrlejglkerj"));
			
		}
		catch (StorageException e)
		{
			Assert.fail("setProperty fails with message:\n" + e.getMessage());
		}
	}
	public void testSetProperty()
	{
		Map<String, String> properties = new HashMap<String, String>();
		
		try
		{
			_property.setProperty("branches/properties/file", "Property file", "15", false);
			properties = _property.getProperties("branches/properties/file");
			
			assertEquals("15", properties.get("Property file"));
			
			//System.out.println("\n== 1 TestPropertiy ======================================\n");
			//System.out.println("\n *** I've just set the property in the repository ***\n\n" +
			//		_property.getProperties("branches/properties/file"));
		}
		catch (StorageException e)
		{
			Assert.fail("setProperty fails with message:\n" + e.getMessage());
		}
	}
	
	public void testGetProperties() throws StorageException
	{
		Map<String, String> properties = new HashMap<String, String>();
		
		_property.setProperty("branches/properties/file", "Property file", "15", false);
		
		properties = _property.getProperties("branches/properties/file");
		
		assertEquals("15", properties.get("Property file"));
		
		//System.out.println("\n== 2 TestProperty ======================================\n");
		//System.out.println("\n *** I set and get the property from the repository ***\n\n" +
		//		properties);
		
//			fail("");   *** Fails a test with the given message
		
	}
	
	
	/*
	 * I have a file in the repository and not in the Working Copy
	 * (I deleted it with deleteFile that deletes file only in the 
	 * Working Copy) so I can get Properties only in that file
	 * 
	 */
	
	public void testNoWCProperty()
	{
		try
		{
			Map<String, String> properties = new HashMap<String, String>();
			_property.getProperties("branches/properties/file");
			_property.deleteFile("branches/properties/file", true );
			properties = _property.getProperties("branches/properties/file");
			//fail(" *** No Property because they have been deleted by me ***");
			
			assertEquals(null , properties.get("Property file"));
			
			//System.out.println("\n== 3 TestProperty ======================================\n");
			//System.out.println("\n *** I have a file in the repository and not in the Working Copy" +
			//		"\n *** (I deleted it with deleteFile that deletes file only in the" +
			//		"\n *** Working Copy) so I can get Properties only in that file\n\n" +
			//		properties);
			
		}
		catch (StorageException e)
		{
			Assert.fail(" getProperty or deleteFile fail with message:\n" + e.getMessage());
			//assertTrue(true);
		}
	}
	
	/*
	 * Now I create a file in WC
	 * It isn't in the repository
	 * set the property for it
	 * and I'll try to get the property from it
	*/
	
	public void testWCProperty() throws IOException, StorageException
	{
		Map<String, String> properties = new HashMap<String, String>();
		_property.storeFile("file2").close();
		_property.setProperty("file2", " *** Property file2 *** ", " - 1 - ", false);
		properties = _property.getProperties("file2");
		
		assertEquals(" - 1 - ", properties.get(" *** Property file2 *** "));
		
		//System.out.println("\n== 4 TestProperty ======================================\n");
		//System.out.println("\n *** Now I create a file in WC \n" +
		//		" *** It isn't in the repository \n" +
		//		" *** set the property for it \n" +
		//		" *** and I get the property from it\n\n" +
		//		properties);
	}
	
	/*
	 * I get the property from a file that is only in the WC
	 */
	public void testWcNoProperty() throws IOException, StorageException
	{
		Map<String, String> properties = new HashMap<String, String>();
				
		_property.storeFile("branches/properties/file3").close();
		properties = _property.getProperties("branches/properties/file3");
		
		assertEquals(null, properties.get(null));
		
		//System.out.println("\n== 6 TestProperty ======================================\n");
		//System.out.println("\n\n *** I get the property from a file that is only in the WC ***\n" 
		//		+ properties);
		
	}
	
	public void testModifyPropertyRepository() 
	{
		Map<String, String> properties = new HashMap<String, String>();
				
		try {
			_property.setProperty("branches/properties/file", "Property file that will be changed",
					"16", false);
			properties = _property.getProperties("branches/properties/file");
		} catch (StorageException e) {
			Assert.fail(" getProperty fails with message:\n" 
					+ e.getMessage());
		}
		
		assertEquals("16", properties.get("Property file that will be changed"));
		
		//System.out.println("\n== 7 TestProperty ======================================\n");
		//System.out.println("\n *** I set and get the property from the repository " +
		//		" **** and I'll modify it in a while ***\n\n" +
		//		properties);
		
		try {
			_property.setProperty("branches/properties/file", " *** Property file just changed ***",
					"17", false);
			properties = _property.getProperties("branches/properties/file");
		} catch (StorageException e) {
			Assert.fail(" setProperty fails with message:\n" 
					+ e.getMessage());
		}
		
		assertEquals("17", properties.get(" *** Property file just changed ***"));
				
		//System.out.println("\n *** I set and get the property from the repository " +
		//		"\n\n" +
		//		properties);
	}
	
	public void testModifyPropertyWC() throws IOException, StorageException
	{
		Map<String, String> properties = new HashMap<String, String>();
		
		_property.storeFile("file4").close();
		_property.setProperty("file4", " *** Property file4 that will be changed *** ", " 17 ", false);
		properties = _property.getProperties("file4");
		
		assertEquals(" 17 ", properties.get(" *** Property file4 that will be changed *** "));
		
		//System.out.println("\n== 8 TestProperty ======================================\n");
		//System.out.println("\n *** Now I create a file in WC \n" +
		//		" *** It isn't in the repository \n" +
		//		" *** set the property for it \n" +
		//		" *** and the property from it are:\n\n" +
		//		properties);
		
		try {
			_property.setProperty("file4", " *** Property file just changed ***",
					" 18 ", false);
			properties = _property.getProperties("file4");
		} catch (StorageException e) {
			Assert.fail(" setProperty fails with message:\n" 
					+ e.getMessage());
		}
		
		assertEquals(" 18 ", properties.get(" *** Property file just changed ***"));
		
		//System.out.println("\n *** I chenged the property of a file in WC: " +
		//		"\n\n" +
		//		properties);
	}
	
}
