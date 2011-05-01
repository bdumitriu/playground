package gw.blog.test;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;

import gw.GwConstants;
import gw.render.MockStorage;
import gw.storage.StorageException;

public class MockBlogStorage extends MockStorage {

	public MockBlogStorage() {
		super();
	}

	public MockBlogStorage(String file) {
		super(file);
	}
	
	public void setExpectedRequestedFile(String requestedFile){ 
		notImplemented();
	} 
	
	/**
     *	 Implementing fileExists for fake entry-files
     */
	public boolean fileExists(String path) throws StorageException {
		if (path.equals("/fakePath/moreFake/entry/Name1/entry.xml") ||
			path.equals("/fakePath/moreFake/entry/Name2/entry.xml") ||
			path.equals("/fakePath/moreFake/entry/Name3/entry.xml") ||
			path.equals("/fakePath/moreFake/entry/Name4/entry.xml"))
			return true;
		
		return false;
	}

	/**
     *	 Implementing isDirectory for fake entry-dirs
     */
	public boolean isDirectory(String path) throws StorageException {
        if (path.equals("/fakePath/moreFake/entry/Name1") ||
        	path.equals("/fakePath/moreFake/entry/Name2") ||
        	path.equals("/fakePath/moreFake/entry/Name3") ||
        	path.equals("/fakePath/moreFake/entry/Name4"))
        	return true;
        
		return false;
	}
	
	public void setUsername(String username) throws StorageException {
		notImplemented();
    }
	
	public void addInputStream(String path, InputStream input) {
		notImplemented();
	}
	
	/**
     *	 Implementing getDirListing for fake entries and comments
     */
	public Iterator getDirListing(String path) throws StorageException{
		if (path.equals("/fakePath/moreFake/com/")){
			LinkedList<String> l = new LinkedList<String>();
			l.add("/fakePath/moreFake/com/Name1");
			l.add("/fakePath/moreFake/com/Name2");
			l.add("/fakePath/moreFake/com/Name3");
			return l.iterator();
		}
		
		if (path.equals("/fakePath/moreFake/entry/")){
			LinkedList<String> l = new LinkedList<String>();
			l.add("/fakePath/moreFake/entry/Name1");
			l.add("/fakePath/moreFake/entry/Name2");
			l.add("/fakePath/moreFake/entry/Name3");
			l.add("/fakePath/moreFake/entry/Name4");
			return l.iterator();
		}
        
        return null;
    }
	
	/**
     *	 Implementing getProperties for fake comments
     */
	public Map getProperties(String path) throws StorageException{
		if(path.equals("/fakePath/moreFake/com/Name1")||
		   path.equals("/fakePath/moreFake/com/Name2")||
		   path.equals("/fakePath/moreFake/com/Name3")){
			HashMap<String,String> map = new HashMap<String,String>();
			map.put("content-type", GwConstants.BLOG_COMMENT_MIME_TYPE);
			return map;
		}
		
        if (path.equals("/blog/testblog")) {
            return new HashMap();
        }
        
        notImplemented();
		return null;
	}
	
	public void setProperty(String path, String name, String value) {
		notImplemented();
	}
    
    public void setProperty(String path, String property, String value, boolean recurse) throws StorageException {
        
    }


	/**
     *	 Implementing getFile to return fake entries and comments
     */
	public InputStream getFile(String path) throws StorageException {
		if(path.equals("/fakePath/moreFake/com/Name1")) {
            return new ByteArrayInputStream( (new String("<comment><testNode>GWRules</testNode></comment>")).getBytes());
        } else if(path.equals("/fakePath/moreFake/com/Name2")) {
            return new ByteArrayInputStream( (new String("<comment><testNode>qwerty</testNode></comment>")).getBytes());
        } else if(path.equals("/fakePath/moreFake/com/Name3")) {
            return new ByteArrayInputStream( (new String("<comment><testNode>Name3</testNode></comment>")).getBytes());
        } else if (path.equals("/fakePath/moreFake/entry/Name1/entry.xml")) {
        	return new ByteArrayInputStream( (new String("<entry><testNode>Entry1</testNode></entry>")).getBytes());
        } else if (path.equals("/fakePath/moreFake/entry/Name2/entry.xml")) {
        	return new ByteArrayInputStream( (new String("<entry><testNode>Entry2</testNode></entry>")).getBytes());
        } else if (path.equals("/fakePath/moreFake/entry/Name3/entry.xml")) {
        	return new ByteArrayInputStream( (new String("<entry><testNode>Entry3</testNode></entry>")).getBytes());
        } else if (path.equals("/fakePath/moreFake/entry/Name4/entry.xml")) {
        	return new ByteArrayInputStream( (new String("<entry><testNode>Entry4GWRULEZZZ</testNode></entry>")).getBytes());
        } else if (path.equals("/testPath/comment.xml")) {
        	return new ByteArrayInputStream( (new String("<?xml version=\"1.0\" encoding=\"UTF-8\"?><comment></comment>")).getBytes());
        } else if (path.equals("/testPath/entry.xml")) {
        	return new ByteArrayInputStream( (new String("<?xml version=\"1.0\" encoding=\"UTF-8\"?><entry></entry>")).getBytes());
        } else if (path.equals("/testPath/xmlfactTest.xml")) {
        	return new ByteArrayInputStream( (new String(
        			"<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
	    			"<entry>" +
	    			"<title>Test title</title>" +
	    			"<date>Mon Oct 31 12:35:42 CEST 2005</date>" +
	    			"<author>FIXME</author>" +
	    			"<content>none</content>" +
	    			"</entry>")
	    			).getBytes());
        } 
        
        if (path.equals("/blog/testblog/entry.xml")) {
            return new ByteArrayInputStream("<entry><title>Blah</title><author>Jan</author><date></date><content></content></entry>".getBytes());
        }
        
        notImplemented();
		return null;
	}
}
