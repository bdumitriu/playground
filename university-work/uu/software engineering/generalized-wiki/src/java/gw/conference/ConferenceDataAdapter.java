package gw.conference;

import gw.conference.exceptions.InvalidDateException;
import gw.storage.Storage;
import gw.storage.StorageException;

import java.util.List;
import java.util.Map;

public class ConferenceDataAdapter {	
	private final Map<String,String> _properties;	
	private final Storage _storage;
	private final String _path;
	
	public ConferenceDataAdapter(Storage storage, String path, Map<String,String> properties) {
		assert properties != null;
        _storage = storage;
        _path = path.endsWith("/") ? path : path + "/";
		_properties = properties;
	}	
	public ConferenceDataAdapter(Storage storage, String path) throws StorageException {
		this(storage, path, storage.getProperties(path));
	}

    /** Returns the associated path, with an ending slash. */
    public String getPath() {
        return _path;
    }
    
    public Storage getStorage() {
        return _storage;   
    }
    
    /**
     * Sets the specified property.
     * @throws NullPointerException If name or value is null.
     */
    public void set(String name, String value) throws StorageException {
        if(value == null) throw new NullPointerException("Property value should not be null for property " + name);

        _properties.put(name, value);
        _storage.setProperty(_path, name, value, false);
    }
    
    /** Returns a property from the internal cache (or null if it doesn't exist). */
    public String get(String name) {
        return _properties.get(name);
    }
    
    public void setList(String name, List<String> strings) throws StorageException {
        StringBuilder builder = new StringBuilder();
        
        for(String s : strings) {
            builder.append(s);
            builder.append(',');
        }
        
        set(name, builder.toString());        
    }
    
    /** Returns a string list property (or an empty list if it doesn't exist). */
    public String[] getList(String name) {
        String value = get(name);
        return value == null ? new String[0] : value.split(",");
    }
    
    protected ConferenceDate getDate(String name) {
        String value = get(name);
        
        if(value == null) return ConferenceDate.getEndOfTime();
        
        try {
            return ConferenceDate.parse(value);
        } catch (InvalidDateException ide) {
            return ConferenceDate.getEndOfTime();
        }
    }
    
    protected void setDate(String name, ConferenceDate value) throws StorageException {
        String textValue = value.toString();
        set(name, textValue);
    }
}
