package gw.render.locator;

import gw.GwContext;
import gw.GwSessionContext;

import gw.render.parsers.ParseException;
import gw.render.StylesheetCreateException;
import gw.render.TransformationResource;
import gw.storage.StorageException;

public interface ResourceLocator {
	
	public TransformationResource locate(GwContext context, GwSessionContext sessioncontext, String pathinfo) throws StylesheetCreateException, StorageException, ParseException;
	
}
