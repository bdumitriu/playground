package gw.storage.javasvn;

import junit.framework.TestCase;

public class PathTest extends TestCase
{
	
	public void testToString()
	{
		assertEquals("/", (new Path("").toString()));
		assertEquals("/home/gideon", (new Path("/home/gideon").toString()));
		assertEquals("/home/gideon", (new Path("/home///gideon").toString()));
		assertEquals("/home/gideon", (new Path("///home/gideon//").toString()));
	}
	
	public void testToStringBoolean()
	{
		assertEquals("", (new Path("").toString(true)));
		assertEquals("home/gideon", (new Path("/home/gideon").toString(true)));
		assertEquals("home/gideon", (new Path("/home///gideon").toString(true)));
		assertEquals("home/gideon", (new Path("///home/gideon//").toString(true)));
	}

	
	public void testToArray() 
	{
		// test empty string
		assertEquals(0, (new Path("")).toArray().length);
		
		// test slash string
		assertEquals(0, (new Path("/")).toArray().length);
		
		// test a longer path
		Path p = new Path("root/somedir/blah");
		String[] array = p.toArray();
		assertEquals("root", array[0]);
		assertEquals("somedir", array[1]);
		assertEquals("blah", array[2]);
		assertEquals(3, array.length);
	}

	public void testPop()
	{
		// test empty path
		Path p = new Path("");
		assertTrue(p.pop() == null);
		assertEquals("/", p.toString());
		
		// test string with one item
		p = new Path("dir");
		assertEquals("dir", p.pop());
		assertEquals("/", p.toString());
		
		// test string with multiple items
		p = new Path("/dir/somedir/otherdir");
		assertEquals("otherdir", p.pop());
		assertEquals("/dir/somedir", p.toString());
		assertEquals("somedir", p.pop());
		assertEquals("/dir", p.toString());
	}

	public void testRemoveSlashes() 
	{
		// test basic cases
		assertEquals("", Path.removeSlashes(""));
		assertEquals("", Path.removeSlashes("/"));
		assertEquals("", Path.removeSlashes("///"));
		
		// test longer paths
		assertEquals("bdsj/kjls/jlklj", Path.removeSlashes("bdsj/kjls/jlklj"));
		assertEquals("bdsj/kjls/jlklj", Path.removeSlashes("bdsj///kjls//jlklj/"));
		assertEquals("bdsj/kjls/jlklj", Path.removeSlashes("//bdsj//kjls/jlklj"));
		assertEquals("bdsj/kjls/jlklj", Path.removeSlashes("///bdsj///kjls//jlklj//"));
	}

	public void testRemoveParents() 
	{
		// test empty path
		assertEquals("", Path.removeParents(""));
		
		// test some simple cases
		assertEquals("somedir", Path.removeParents("somedir/"));
		assertEquals("somedir", Path.removeParents("/somedir/"));
		
		// test some longer cases
		assertEquals("somedir", Path.removeParents("/anydir/blah/somedir/"));
		assertEquals("somedir", Path.removeParents("anydir/blah/somedir/"));
	}
}
