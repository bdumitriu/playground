package gw.storage.javasvn.virtualwc;

import gw.storage.javasvn.Path;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

public class StoreFileOutputStream extends FileOutputStream
{
	private File newFile;
	private Path path;
	private boolean isClosed;
	private AddManager adder;
	
	public StoreFileOutputStream(File newFile, Path path, AddManager manager)
			throws FileNotFoundException
	{
		super(newFile);
		
		this.newFile = newFile;
		this.path = path;
		this.isClosed = false;
		this.adder = manager;
	}
	
	@Override
	public void close() throws IOException
	{
		super.close();
		
		if ( !isClosed )
		{
			try {
				adder.storeFile(path, newFile);
			}
			catch ( Exception e )
			{
				IOException out = new IOException();
				out.initCause(e);
				throw out;
			}
			
			isClosed = true;
		}
	}

}
