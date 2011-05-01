package gw.storage;

import java.util.List;
import java.util.ArrayList;

public class StorageListenerContainer
{
    private static ArrayList<StorageListener> _listeners = new ArrayList<StorageListener>();
    
    public static void addListener(StorageListener listener)
    {
        _listeners.add(listener);
    }

    public static void removeListener(StorageListener listener)
    {
        _listeners.remove(listener);
    }

    static List<StorageListener> getListeners()
    {
        return (List<StorageListener>) _listeners.clone();
    }
}
